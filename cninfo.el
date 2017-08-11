(require 's)
(require 'request)

(defvar current-report-url nil)
(defvar target-dir "~/annualReports/")
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
  (concat target-dir "r" code "-2016.PDF"))

(defun get-report (code)
  (unless (file-exists-p (local-report-file code))
    (progn (get-annual-report-url code)
	   ;;wait for the response
	   (sleep-for 5)
	   (download-file
	    (concat url-base (car current-report-url))
	    (local-report-file code))))
  (setq cur-pdf (local-report-file code))
  (setq cur-page 1)
  (page cur-page))

;;delete the old report and fetch it again
(defun reget-report (code)
  (if (file-exists-p (local-report-file code))
      (delete-file (local-report-file code)))
  (get-report (code)))


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
  
  


(global-set-key (kbd "C-c C-p") 'prev-page)
(global-set-key (kbd "C-c C-n") 'next-page)
(global-set-key (kbd "C-c C-g") 'go-page)
(global-set-key (kbd "C-c C-x C-p") 'i-get-report)

(provide 'cninfo)


