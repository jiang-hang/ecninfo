;; the report view mode
;; basic function
;; 1. grep "尿素" *.txt
;; 2. show the results , save the keyword,
;; during the review , re-grep specified file
;; with the grep option -C , capture the results
;;

(defvar current-keyword "尿素")
(defvar current-report nil)
(defvar context-line-num 1)
(defvar bankuai nil)

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
  "grep the current-keyword"
  (interactive "P")
  (shell-command (concat "grep -H "
			 current-keyword
			 (if (= 0 context-line-num)
			     " "
			   (concat  " -C "
				    (number-to-string context-line-num)))
			 " -n "
   		         " *txt ")
		 (get-buffer "report-view"))
  (with-current-buffer (get-buffer "report-view")
    (report-highlight-key)))


(defun report-grep-show-files ()
  "show the matched files at the buffer"
  (interactive)
  (with-current-buffer (get-buffer-create "report-view-files")
    (shell-command (concat "grep -l "
			   current-keyword
			   " *txt ")
		   (current-buffer))
    (reportview-mode)))

(defun re-grep-the-file ()
  "re-grep the report under the cursor"
  (interactive)
  (let ((cur-report (car (split-string (thing-at-point 'filename) ":")))
	(num context-line-num))
    (setq current-report cur-report)
    (shell-command (concat "grep "
			   current-keyword
			   " -n -H -C "
			   (number-to-string  num)
			   " "
			   cur-report)
		   (get-buffer "report-view")))
  (with-current-buffer (get-buffer "report-view")
    (report-highlight-key)))

(defun open-pdf-file ()
  (interactive)
  (unless current-report
    (error "no current-report is set"))
  (let ((code (substring current-report 1 7)))
    (shell-command (concat "evince "
			   (local-report-file code)))))


(defun get-section (&optional arg)
  "show the interested sections"
  (interactive "Nplease select the item[1]:公司业务概要[2]:核心竞争力")
  (cond 
	((= arg 1)  (shell-command (concat "grep -E -n -H -A 50 '公司业务概要' "
				   current-report)
			   (current-buffer)))
	((= arg 2)  (shell-command (concat "grep -E -n -H -A 50 '核心竞争力' "
				   current-report)
			   (current-buffer)))))


(defun report-highlight-key (&optional arg)
  (unhighlight-regexp current-keyword)
  (highlight-regexp current-keyword 'hi-green))

(defun add-keyword-to-bankuai ()
  "adding the current keyword to bankuai"
  (unless (assoc current-keyword bankuai)
    (push `(,current-keyword) bankuai)))

(defun add-code-to-bankuai ()
  (interactive)
  "adding the code in current line to the current keyword bankuai"
  (unless (assoc current-keyword bankuai)
    (add-keyword-to-bankuai))
  (beginning-of-line)
  (let ((code (buffer-substring (+ 1 (point)) (+ 7 (point)))))
    (unless (memq code (cdr (assoc current-keyword bankuai)))
      (setcdr (assoc current-keyword bankuai)
	      (cons code (cdr (assoc current-keyword bankuai)))))))

(defun load-groups ()
  (with-temp-buffer
    (insert-file-contents (concat project-root "groups.el"))
    (goto-char (point-min))
    (setq bankuai (read (current-buffer)))))
	       
(defun save-groups ()
  (interactive)
  (with-temp-file (concat project-root "groups.el")
    (print bankuai (current-buffer))))

;;  (defun show-windows ()
;;    (interactive)
;;    (let* ((keywords-window
;; 	   (split-window (car (window-list)) (round (* 0.85 (window-total-width))) 'left))
;; 	  (codes-window
;; 	   (split-window keywords-window (round (* 0.35 (window-total-height keywords-window))) 'below)))
;;    (set-window-buffer keywords-window
;;  		     (get-buffer "keywords"))
;;    (set-window-buffer codes-window
;; 		      (get-buffer "codes"))))

;; (defun report-view-init ()
;;   (interactive)
;;   (show-windows))

(defun fresh-keywords-buffer ()
  (with-current-buffer (get-buffer "keywords")
    (delete-region (point-min) (point-max))
    (loop for x in bankuai
	  do (if (string= (car x) current-keyword)
		 (print (concat "*" (car x)) (current-buffer))
	       (print (car x) (current-buffer))
	       ))))

(defun move-and-grep ()
  (interactive)
  (with-current-buffer (get-buffer "report-view-files")
    (next-line)
    (re-grep-the-file)))

(defun report-view ()
  (interactive)
  (setq default-directory "~/annualReports/txt/")
  (let ((buffer (get-buffer-create "report-view")))
    (get-buffer-create "keywords")
    (get-buffer-create "codes")
    (load-groups)
    (with-current-buffer buffer
      (reportview-mode))

    (switch-to-buffer buffer)))

  

(define-derived-mode
  reportview-mode
  special-mode
  "reportview"
  "Major mode for annual report view")

(define-key reportview-mode-map   "g" 'report-grep)
(define-key reportview-mode-map   "G" 'report-grep-show-files)
(define-key reportview-mode-map   "c" 're-grep-the-file)
(define-key reportview-mode-map   "k" 'set-report-keyword)
(define-key reportview-mode-map   "b" 'get-section)
(define-key reportview-mode-map   "*" 'grep-word-on-region)
(define-key reportview-mode-map   "n" 'set-context-line-num)
(define-key reportview-mode-map   "s" 'save-groups)
(define-key reportview-mode-map   "a" 'add-code-to-bankuai)
(define-key reportview-mode-map   "p" 'open-pdf-file)
(define-key reportview-mode-map   (kbd "<f5>") 'move-and-grep)



(provide 'repview)

			     
  
		   
