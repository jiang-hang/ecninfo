(require 's)
(require 'request)
(require 'enlive)

;;codes is defined there
(require 'allcodes)

(defvar current-report-url nil)
(defvar target-dir "/home/xuyang/annualReports/")
(defvar report-year 2016)
(defvar url-base "http://www.cninfo.com.cn")
(defvar cur-page 1)
(defvar cur-pdf nil)

(defvar project-root "/home/xuyang/ecninfo/")


(defun update-target-dir (y)
  (setq target-dir
	(concat "/home/xuyang/annualReports/"
		(number-to-string y) "/")))

(defun set-report-year (y fetch-or-view)
  (interactive "ninput the year for report (eg:2017):\nnthe 1 for fetch and 2 for view")
  (setq report-year y)
  (update-target-dir report-year)
  (setq default-directory target-dir)
  (if (= 2 fetch-or-view)
      (setq default-directory (concat target-dir "txt/"))))


(defun get-annual-report-url (code)
  (request
;;;   "http://www.cninfo.com.cn/search/search.jsp"
   "http://www.cninfo.com.cn/search/stockfulltext.jsp"
 :type "POST"
 :data `(("noticeType" . "010301")
	 ("stockCode" . ,code)
	 ("startTime" . ,(concat (number-to-string (+ 1 report-year)) "-01-01"))
	 ("endTime" . ,(concat (number-to-string (+ 1 report-year)) "-07-01"))
	 ("Submit1.x" . "26")
	 ("Submit1.y" . "10")
	 )
 :parser (lambda ()
	   (decode-coding-region (point-min) (point-max) 'gbk)
	   (libxml-parse-html-region (point-min) (point-max)))
 :success
 (cl-function
  (lambda (&key data &allow-other-keys)
    (setq current-report-url
     (loop for x in (enlive-query-all data [td.qsgg > a])
	   unless (s-contains? "摘要" (enlive-text x))
	   collect (enlive-attr x 'href)))
  current-report-url))))


(defun download-file (url target)
  (shell-command
   (concat "wget " "-O " target " " url))) 

(defun local-report-file (code)
  (concat target-dir "pdf/r" code "-" (number-to-string report-year)
	  ".PDF"))

(defun local-report-exists? (code)
  (if (file-exists-p (local-report-file code))
      t
    nil))


(defun local-gaiyao-file (code)
  (concat target-dir "gaiyao/gy" code "-"
	  (number-to-string report-year) ".txt"))

(defun get-report (code)
  (unless (local-report-exists? code)
    (progn (get-annual-report-url code)
	   ;;wait for the response
	   (sleep-for 8)
	   (download-file
	    (concat url-base (car current-report-url))
	    (local-report-file code))))
  (setq cur-pdf (local-report-file code))
  (setq cur-page 1)
  (page cur-page))



;;delete the old report and fetch it again
(defun reget-report (code)
  (if (local-report-exists? code)
      (delete-file (local-report-file code)))
  (get-report code))


(defun page (nu)
  (with-current-buffer (get-buffer-create "pdf-report")
    (delete-region (point-min) (point-max))
    (shell-command (concat "pdf2txt " "-p "
			   (number-to-string nu)
			   " "
			   cur-pdf)
		   (get-buffer "pdf-report"))))


  
(defun prev-page (&optional n)
  (interactive "p")
  (unless n (setq n 1))
  (if (> cur-page n)
      (progn (setq cur-page (- cur-page n))
	     (page cur-page))))

(defun next-page (&optional n)
  (interactive "p")
    (unless n (setq n 1))
      (setq cur-page (+ cur-page n))
      (page cur-page))

(defun go-page (nu)
  (interactive "ngo to page[1]:")
  (if nu (page nu)))


(defun i-get-report (code)
  (interactive "sinput the code like 600309:")
  (get-report code))

(defun get-report-and-wait (code)
  (unless (local-report-exists? code)
    (get-report code)
    (sleep-for 25)))

(defun verify-report (code)
  (let ((exists
	 (if (local-report-exists? code)
	     (progn (with-current-buffer (get-buffer-create "pdf-verify")
		      (delete-region (point-min) (point-max))
		      (shell-command (concat "pdftotxt " " -f 1 -l 20 "
					     (local-report-file code))
				     (get-buffer "pdf-verify"))
		      (goto-char (point-min))
		      (search-forward code nil t))))))
    (if exists
	(progn (message "%s is verified" code) 1)
      (progn (message "%s will be refreshed..." code)
	     (reget-report code)
	     (sleep-for 25)
	     nil))))

(defun local-txt-report (code)
  (car (directory-files (concat target-dir "txt/")
		   t
		   (concat "r" code ".*"))))

;;(defun local-txt-report-with-name (code)
;;  (concat target-dir "txt/r" code 


(defun local-txt-report-exists? (code)
  (if (and
       (local-txt-report code)
       (file-exists-p (local-txt-report code)))
      t
    nil))

(defun local-txt-report-with-name (code)
  ;;get the name firstly
  (concat target-dir "txt/r" code "-" (number-to-string report-year) "-" 
	  (cdr (assoc code codes))
	  ".txt"))

(defun pdf-to-txt (code)
  (when (local-report-exists? code)
    (unless (local-txt-report-exists? code)
      (message "process %s" code)
      (shell-command (concat "pdftotext "
			     (local-report-file code)
			     " "
			     (local-txt-report code))))))

(defun rename-txt (code)
  (when (local-txt-report-exists? code)
    (unless (string= (local-txt-report code)
		     (local-txt-report-with-name code))
      (rename-file (local-txt-report code)
		   (local-txt-report-with-name code)))))

      
		     

(defun collect-gaiyao (code)
  (interactive "sinput the code like 600309:")
  (unless (local-report-exists? code)
      (progn (get-report code)
	     (sleep-for 15)))
  (with-current-buffer  (get-buffer-create "pdf-report-raw")
    (delete-region (point-min) (point-max))
    (shell-command (concat "pdf2txt -m 50 " 
			 (local-report-file code))
		   (get-buffer "pdf-report-raw"))
    (goto-char (point-min))
    (let ((pos1 (search-forward "公司业务概要" nil t 2))
	  (pos2 (search-forward "经营情况讨论与分析" nil t 1)))
      (copy-to-buffer (get-buffer-create "pdf-report")
		      pos1 pos2)))
  (with-current-buffer  (get-buffer "pdf-report")
    (write-region (point-min) (point-max) (local-gaiyao-file code))
    (kill-buffer)))


;; (global-set-key (kbd "C-c C-p") 'prev-page)
;; (global-set-key (kbd "C-c C-n") 'next-page)
;; (global-set-key (kbd "C-c C-g") 'go-page)
;; (global-set-key (kbd "C-c C-x C-p") 'i-get-report)

(require 'repview)

(provide 'cninfo)
