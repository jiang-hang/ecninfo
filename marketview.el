(require 's)
(require 'request)
(require 'enlive)

;;codes is defined there
(require 'allcodes)


;;format ?list=sh600038,sz000001
(defvar sina-base "http://hq.sinajs.cn/?list=")

(defun code-with-market (code)
  (concat (if (< (string-to-number code) 600000) "sz" "sh") code))

(defun get-price (code)
  (let ((ret 0.0))
    (request
     (concat sina-base (code-with-market code))
     :type "GET"
     :parser (lambda ()
	       (decode-coding-region (point-min) (point-max) 'gbk)
	       (buffer-string))
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
	(let ((strs (split-string data ",")))
	  (setq ret (nth 3 strs)))
	))))
  (sleep-for 2)
  (string-to-number ret))

(defun get-pricex (code-list retindex)
  (let ((ret 0.0))
    (request
     (concat sina-base
	     (mapconcat #'code-with-market code-list ","))
     :type "GET"
     :parser (lambda ()
	       (decode-coding-region (point-min) (point-max) 'gbk)
	       (message "xx %s" (buffer-string))
	       (buffer-string))
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
	      (let* ((splited (split-string data ","))
		     (len (length splited))
		     (ssize (/ (- len 1) 32))
		     )
		(setq ret (string-to-number
			   (nth retindex (mapcar #'(lambda (idx)
			    (nth (+ 3 (* 32 idx)) splited))
						 (number-sequence 0 (- ssize 1))))))))))
    (sleep-for 2) ;;wait for the response , or errors will happen
    (identity ret)))

          
(provide 'markedview)   
  
