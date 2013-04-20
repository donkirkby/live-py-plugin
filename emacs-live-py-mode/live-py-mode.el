(defun live-py-after-change-function (start stop len)
  "Run the buffer through the code tracer and show results in the trace buffer"
  (when live-py-timer (cancel-timer live-py-timer))
  (set 'live-py-timer (run-at-time 0.5 nil 'live-py-trace)))


(defun live-py-trace ()
  (let* ((tracer-path (locate-file "code_tracer.py" load-path))
         (buffer-dir (file-name-directory (buffer-file-name)))
         (command-line (concat "python " tracer-path))
         (pythonpath (concat "PYTHONPATH=" buffer-dir))
         (process-environment (cons pythonpath process-environment))
         )
    (shell-command-on-region 1
                             (+ (buffer-size) 1)
                             command-line
                             live-py-output-buffer))
  (with-current-buffer live-py-output-buffer
    (setq buffer-read-only 1))
  (live-py-synchronize-scroll)
  (set 'live-py-timer nil))


(defun live-py-synchronize-scroll ()
  (let ((code-window-start (+ (count-lines 1 (window-start)) 1))
        (position (line-number-at-pos)))
    (unless (window-valid-p live-py-output-window)
      (live-py-show-output-window))
    (with-selected-window live-py-output-window
      (goto-line code-window-start)
      (recenter-top-bottom 0)
      (goto-line position))))


(defun live-py-check-to-scroll ()
  "Check `this-command' to see if a scroll is to be done."
  (cond ((memq this-command '(next-line
                              previous-line
                              scroll-up
                              scroll-up-command
                              scroll-down
                              scroll-down-command
                              beginning-of-buffer
                              end-of-buffer))
         (live-py-synchronize-scroll))))


(defun live-py-show-output-window ()
  (delete-other-windows)
  (get-buffer-create live-py-output-buffer)
  (with-current-buffer live-py-output-buffer
    (toggle-truncate-lines 1))
  (set (make-local-variable 'live-py-output-window)
       (split-window-horizontally))
  (set-window-buffer live-py-output-window live-py-output-buffer))


(define-minor-mode live-py-mode
  "Minor mode to do on-the-fly Python tracing.
When called interactively, toggles the minor mode.
With arg, turn mode on if and only if arg is positive."
  :group 'live-py-mode :lighter live-py-mode-line
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (cond

   ;; Turning the mode ON.
   (live-py-mode
    ;; create a unique name for the trace output buffer
    (set (make-local-variable 'live-py-output-buffer)
         (concat "*live-py-output_"
                 (file-name-nondirectory (buffer-file-name))
                 "_"
                 (make-temp-name "")
                 "*"))
    (set (make-local-variable 'live-py-timer) nil)
    (add-hook 'after-change-functions 'live-py-after-change-function nil t)
    (live-py-show-output-window)
    (live-py-after-change-function 0 0 0)
    (add-hook 'post-command-hook 'live-py-check-to-scroll nil t)
    )
   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions 'live-py-after-change-function t)
    (remove-hook 'post-command-hook 'live-py-check-to-scroll t)
    (kill-buffer live-py-output-buffer)
    (delete-window live-py-output-window)
    )
   )
  )

(provide 'live-py-mode)
