;;; live-py-mode.el --- Live Coding in Python

;; Copyright (C) 2015 Don Kirkby

;; Author: Don Kirkby <donkirkby@gmail.com>
;; Keywords: live coding
;; URL: http://donkirkby.github.io/live-py-plugin/
;; Version: 2015
;; X-Original-Version: 2.8.1
;; Package-Requires: ((emacs "24.1"))

;; This program is distributed under the Eclipse Public License - v 1.0
;; For more information see https://www.eclipse.org/legal/epl-v10.html

;;; Commentary:

;; To use it, open a Python file and run M-x live-py-mode
;; If that doesn't work, put the following in your Emacs configuration file:
;;
;;   (require 'live-py-mode)
;;
;; Requirements: Emacs 24.

;;; Code:

(defvar live-py-timer)
(defvar live-py-output-buffer nil "The name of the output buffer.")
(defvar live-py-output-window)
(defvar live-py-driver)
(defvar live-py-module)
(defvar live-py-parent)
(defvar live-py-dir)
(defvar live-py-path)
(defvar live-py-version)
(defvar live-py-window-start-pos)
(defvar live-py-point-line-nr)
(defvar live-py-lighter)

(defun live-py-after-change-function (start stop len)
  "Run the buffer through the code tracer and show results in the trace buffer."
  (when live-py-timer (cancel-timer live-py-timer))
  (set 'live-py-timer (run-at-time 0.5 nil 'live-py-trace)))

(defun live-py-trace ()
  "Trace the Python code using code_tracer.py."
  (let* ((tracer-path (locate-file "code_tracer.py" load-path))
         (buffer-dir (file-name-directory (buffer-file-name)))
         (command-line-start (concat
			      (shell-quote-argument live-py-version)
			      " "
			      (shell-quote-argument tracer-path)
			      " -f "
			      (shell-quote-argument buffer-file-name)))
	 (command-line (if live-py-driver
			   (concat
			    command-line-start
			    " - "
			    live-py-module
			    " "
			    live-py-driver)
			   command-line-start))
         (pythonpath (concat "PYTHONPATH=" (shell-quote-argument
					    (or live-py-path live-py-dir))))
         (process-environment (cons pythonpath process-environment))
         (default-directory live-py-dir)
         (reused-buffer (buffer-live-p
                         (and live-py-output-buffer
                              (get-buffer live-py-output-buffer)))))
    (save-restriction
      (widen)
      ;; Create (or recreate) if necessary, update and last but not least
      ;; display output buffer.
      (if (eq 0
              (shell-command-on-region 1
                                       (1+ (buffer-size))
                                       command-line
                                       live-py-output-buffer))
          (setq live-py-lighter " live")
        (setq live-py-lighter " live(FAIL)")))
    (with-current-buffer live-py-output-buffer
      (setq buffer-read-only 1)
      (unless reused-buffer (toggle-truncate-lines 1)))
    (live-py-synchronize-scroll
     (line-number-at-pos (window-start)) (line-number-at-pos))
    (set 'live-py-timer nil)))

(defun live-py-synchronize-scroll (window-start-line-nr point-line-nr)
  "Synchronize scrolling between Python and output buffer.

Pass the possibly reused (line-number-at-pos (window-start)) to
WINDOW-START-LINE-NR and (line-number-at-pos) to POINT-LINE-NR,
both are relative to (point-min). Numbering starts at 1 for all
*-LINE-NR in this function signature and body.

When the Python buffer is narrowed the output buffer remains
aligned but will not hide the part after the narrowing."
  (let ((point-min-pos (point-min))
        (point-min-line-nr 1))
    (unless (= 1 point-min-pos)
      ;; Compensate for narrowing.
      (save-restriction
        (widen)
        (setq point-min-line-nr (line-number-at-pos point-min-pos))))
    (unless (window-valid-p live-py-output-window)
      ;; Recreate output window.
      (live-py-show-output-window))
    (with-selected-window live-py-output-window
      (goto-char 1)
      (forward-line (+ point-min-line-nr window-start-line-nr -2))
      (recenter-top-bottom 0)
      (forward-line (- point-line-nr window-start-line-nr)))))

(defun live-py-check-to-scroll ()
  "Check if window start or point have to be synchronized."
  ;; Take extra care to not let this function run into a non-handled error.
  ;; On such an error the debugger will not be entered to not block Emacs
  ;; interactivity when `debug-on-error' is active, so it is easily possible
  ;; to miss the error. And on such an error the function will be removed
  ;; from `post-command-hook' which can be quite confusing.

  ;; `window-start' is for some reason not up to date after
  ;; `post-command-hook' in at least these situations:
  ;; - For a few commands like `narrow-to-region' or `viper-goto-line'.
  ;; - Repeated `next-line' towards the end of the Python buffer: The output
  ;;   buffer suddenly lags one line behind.
  ;; See also "calculating new window-start/end without redisplay"
  ;; http://stackoverflow.com/questions/23923371 )
  (when (memq this-command '(narrow-to-region next-line viper-goto-line))
    (redisplay))
  (let ((window-start-pos (window-start))
        (point-line-nr (line-number-at-pos)))
    (unless (and (= live-py-window-start-pos window-start-pos)
                 (= live-py-point-line-nr point-line-nr))
      (set (make-local-variable 'live-py-window-start-pos) window-start-pos)
      (set (make-local-variable 'live-py-point-line-nr) point-line-nr)
      (if (buffer-live-p (and live-py-output-buffer
                              (get-buffer live-py-output-buffer)))
          (progn
            (unless (window-valid-p live-py-output-window)
              ;; Recreate output window.
              (live-py-show-output-window))
            (set-window-buffer live-py-output-window live-py-output-buffer)
            (live-py-synchronize-scroll
             (line-number-at-pos window-start-pos) point-line-nr))
        ;; Recreate output buffer and if necessary the output window.
        (live-py-trace)))))

(defun live-py-show-output-window ()
  "Show the live-py output window."
  (delete-other-windows)
  (get-buffer-create live-py-output-buffer)
  (toggle-truncate-lines 1)
  (with-current-buffer live-py-output-buffer
    (toggle-truncate-lines 1)
    (setq-local show-trailing-whitespace nil))
  (set (make-local-variable 'live-py-output-window)
       (split-window-horizontally))
  (set-window-buffer live-py-output-window live-py-output-buffer))

(defun live-py-set-driver()
  "Prompt user to enter the driver command, with input history support.
To use a unit test, set the driver to something like this:
-m unittest mymodule.MyTest.test_method"
  (interactive)
  (setq live-py-driver (read-string "Type the driver command:"))
  (live-py-after-change-function 0 0 0))

(defun live-py-set-version()
  "Prompt user to enter the python command, with input history support.
Typical values are 'python' or 'python3'."
  (interactive)
  (setq live-py-version (expand-file-name
                         (read-shell-command "Type the python command:")))
  (live-py-after-change-function 0 0 0))

(defun live-py-set-dir()
  "Prompt user to enter the working directory."
  (interactive)
  (setq live-py-dir (expand-file-name
                     (directory-file-name
                      (read-directory-name
                       "Working directory:"
                       nil
                       nil
                       t))))

  (unless (string-prefix-p live-py-dir buffer-file-name)
    (error "Working directory %s must be a parent of %s." live-py-dir buffer-file-name))
  (setq live-py-module (file-name-base buffer-file-name))
  (setq live-py-parent (directory-file-name
			(file-name-directory buffer-file-name)))
  (while (not (string= live-py-parent live-py-dir))
    (setq live-py-module (concat
			  (file-name-nondirectory live-py-parent)
			  "."
			  live-py-module))
    (setq live-py-parent (directory-file-name
			  (file-name-directory live-py-parent))))
  (setq live-py-dir (file-name-as-directory live-py-dir))
  (live-py-after-change-function 0 0 0)
  (message "Working directory set to %s." live-py-dir))

(defun live-py-set-path()
  "Prompt user to enter extra directories for the Python path."
  (interactive)
  (setq live-py-path (expand-file-name
                      (read-string "PYTHONPATH:"))))

(defun live-py-mode-off ()
  "Wrapper function to turn the mode off."
  (interactive) ; Allow binding to a key
  (live-py-mode 0))

;;;###autoload
(define-minor-mode live-py-mode
  "Minor mode to do on-the-fly Python tracing.
When called interactively, toggles the minor mode.
With arg, turn mode on if and only if arg is positive.
\\{live-py-mode-map}"
  :group 'live-py-mode
  :lighter live-py-lighter
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c M-d") 'live-py-set-driver)
	    (define-key map (kbd "C-c M-w") 'live-py-set-dir)
	    (define-key map (kbd "C-c M-p") 'live-py-set-path)
	    (define-key map (kbd "C-c M-v") 'live-py-set-version)
	    map)
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
    (set (make-local-variable 'live-py-driver) nil)
    (set (make-local-variable 'live-py-module) (file-name-base buffer-file-name))
    (set (make-local-variable 'live-py-dir) (file-name-directory buffer-file-name))
    (set (make-local-variable 'live-py-path) nil)
    (set (make-local-variable 'live-py-version) "python")
    (set (make-local-variable 'live-py-window-start-pos) -1)
    (set (make-local-variable 'live-py-point-line-nr) -1)
    (add-hook 'kill-buffer-hook 'live-py-mode-off nil t)
    (add-hook 'after-change-functions 'live-py-after-change-function nil t)
    (live-py-show-output-window)
    (live-py-after-change-function 0 0 0)
    (add-hook 'post-command-hook 'live-py-check-to-scroll nil t))
   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions 'live-py-after-change-function t)
    (remove-hook 'post-command-hook 'live-py-check-to-scroll t)
    (remove-hook 'kill-buffer-hook 'live-py-mode-off t)
    (ignore-errors (kill-buffer live-py-output-buffer))
    (when (window-valid-p live-py-output-window)
      (delete-window live-py-output-window))
    (toggle-truncate-lines 0))))

(provide 'live-py-mode)

;;; live-py-mode.el ends here
