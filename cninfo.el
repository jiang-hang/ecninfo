(require 's)
(require 'request)

;;codes is defined there
(require 'allcodes)

(defvar current-report-url nil)
(defvar target-dir "/home/xuyang/annualReports/")
(defvar url-base "http://www.cninfo.com.cn")
(defvar cur-page 1)
(defvar cur-pdf nil)

(defun get-annual-report-url (code)
  (request
 "http://www.cninfo.com.cn/search/search.jsp"
 :type "POST"
 :data `(("noticeType" . "010301")
	 ("orderby" . "date11")
	 ("stockCode" . ,code)
	 ("startTime" . "2017-01-01")
	 ("endTime" . "2017-07-01")
	 ("pageNo" . "1"))
 :parser (lambda ()
	   (decode-coding-region (point-min) (point-max) 'gbk)
	   (libxml-parse-html-region (point-min) (point-max)))
 :success
 (cl-function
  (lambda (&key data &allow-other-keys)
    (setq current-report-url
     (loop for x in (enlive-query-all data [td.qsgg > a])
	   unless (s-contains? "摘要" (enlive-text x))
	   collect (enlive-attr x 'href))))))
  current-report-url)


(defun download-file (url target)
  (shell-command
   (concat "wget " "-O " target " " url))) 

(defun local-report-file (code)
  (concat target-dir "pdf/r" code "-2016.PDF"))

(defun local-report-exists? (code)
  (if (file-exists-p (local-report-file code))
      t
    nil))


(defun local-gaiyao-file (code)
  (concat target-dir "gaiyao/gy" code "-2016.txt"))

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
  (if (file-exists-p (local-txt-report code))
      t
    nil))

(defun local-txt-report-with-name (code)
  ;;get the name firstly
  (concat target-dir "txt/r" code "-2016-" 
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


(global-set-key (kbd "C-c C-p") 'prev-page)
(global-set-key (kbd "C-c C-n") 'next-page)
(global-set-key (kbd "C-c C-g") 'go-page)
(global-set-key (kbd "C-c C-x C-p") 'i-get-report)

(provide 'cninfo)
