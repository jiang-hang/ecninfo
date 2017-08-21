;; the report view mode
;; basic function
;; 1. grep "尿素" *.txt
;; 2. show the results , save the keyword,
;; during the review , re-grep specified file
;; with the grep option -C , capture the results
;;

(defvar current-keyword "尿素")
(defvar current-report "")
(defvar context-line-num 1)

(defvar report-keywrods
  `("尿素" "乙二醇" "MDI" "聚碳酸酯"))

(defun set-report-keyword (word)
  (interactive "Minput the keyword:")
  (setq current-keyword word))

(defun set-context-line-num (lines)
  (interactive "Ninput the context line num:")
  (setq context-line-num lines))


(defun grep-word-on-region (begin end)
  (interactive "r")
  (let ((wd (buffer-substring begin end)))
    (setq current-keyword wd)
    (report-grep)))

(defun report-grep (&optional arg)
  (interactive "P")
  (shell-command (concat "grep -H "
			 current-keyword
			 " -C "
			 (number-to-string context-line-num)
			 " -n *txt ")
		 (get-buffer "report-view")))

(defun re-grep-the-file (&optional context-lines)
  "re-grep the report under the cursor"
  (interactive "Ngive the n:")
  (let ((cur-report (car (split-string (thing-at-point 'filename) ":")))
	(num context-lines))
    (setq current-report cur-report)
    (shell-command (concat "grep "
			   current-keyword
			   " -n -H -C "
			   (number-to-string  num)
			   " "
			   cur-report)
		   (current-buffer))))


(defun get-section (&optional arg)
  "show the interested sections"
  (interactive "Nplease select the item[1]:公司业务概要[2]:核心竞争力")
  (cond 
	((= arg 1)  (shell-command (concat "grep -E -n -H -A 50 '公司业务概要$' "
				   current-report)
			   (current-buffer)))
	((= arg 2)  (shell-command (concat "grep -E -n -H -A 50 '核心竞争力$' "
				   current-report)
			   (current-buffer)))))


(defun report-highlight-key (&optional arg)
  (interactive "P")
  (unhighlight-regexp current-keyword)
  (highlight-regexp current-keyword 'hi-green))

(define-derived-mode
  reportview-mode
  special-mode
  "reportview"
  "Major mode for annual report view")

(define-key reportview-mode-map   "g" 'report-grep)
(define-key reportview-mode-map   "c" 're-grep-the-file)
(define-key reportview-mode-map   "k" 'set-report-keyword)
(define-key reportview-mode-map   "a" 'get-section)
(define-key reportview-mode-map   "*" 'grep-word-on-region)
(define-key reportview-mode-map   "n" 'set-context-line-num)
(define-key reportview-mode-map   "s" 'report-highlight-key)

(add-hook 'reportview-mode-hook
	  (lambda ()
	    (setq default-directory "~/annualReports/txt/")
	    (switch-to-buffer (get-buffer-create "report-view"))))
  
  

(provide 'repview)

			     
  
		   
