(defconst sblog:version "0.1")
(defvar sblog:path-command-list '("log"))

(defun sblog:exec (&rest args)
  (apply 'call-process "sblog" nil t nil args))

(defun sblog:ls ()
  (with-temp-buffer
    (sblog:exec "ls")
    (split-string (buffer-substring-no-properties (point-min) (- (point-max) 1)) "\n")))

(defun sblog:ls* ()
  (append sblog:path-command-list (sblog:ls)))

(defun sblog:post ()
  (interactive)
  (let ((x (sblog:ls*)))
    (anything `((name . "sblog post")
                (candidates . ,x)
                (action . (("edit" . sblog:edit)
                           ("rm" . sblog:rm))))))
    (unless (anything-get-selection)
      (sblog:edit (car minibuffer-history))))

(defun sblog:rm (path)
  (with-temp-buffer
    (if (= 0 (sblog:exec "rm" path))
        (message "sblog: remove %s" path)
      (message (buffer-substring-no-properties (point-min) (point-max))))))

(defun sblog:cat (path)
  (with-temp-buffer
    (let ((r (sblog:exec "cat" path)))
      (if (= r 0)
          (buffer-substring-no-properties (point-min) (point-max))
        nil))))

(defun sblog:init-mode (path)
  (make-local-variable 'sblog:flag)
  (make-local-variable 'sblog:current-path)
  (setq sblog:flag t)
  (setq sblog:current-path path)
  (define-key markdown-mode-map "\C-x\C-s" 'sblog:save-buffer))

(defun sblog:modep () (boundp 'sblog:flag))

(defun sblog:save-buffer ()
  (interactive)
  (if (sblog:modep)
      (and (= 0 (call-process-region (point-min) (point-max) "sblog" nil t nil "post"))
           (message "sblog save buffer %s" sblog:current-path))
    (save-buffer)))

(defun sblog:edit (path)
  (let ((bufname (format "*sblog %s*" path)))
    (if (get-buffer bufname)
        (switch-to-buffer bufname)
      (let ((buf (get-buffer-create bufname)))
        (switch-to-buffer buf)
        (markdown-mode)
        (sblog:init-mode path)
        (save-excursion
          (insert (or (sblog:cat path)
                      (with-temp-buffer
                        (sblog:exec "touch" path)
                        (buffer-substring-no-properties (point-min) (point-max))))))))))

(provide 'sblog)
