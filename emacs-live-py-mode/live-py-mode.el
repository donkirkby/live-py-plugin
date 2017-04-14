;;; live-py-mode.el --- Live Coding in Python -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Don Kirkby

;; Author: Don Kirkby <donkirkby@gmail.com>
;; Keywords: live coding
;; URL: http://donkirkby.github.io/live-py-plugin/
;; Version: 2015
;; X-Original-Version: 2.8.1
;; Package-Requires: ((emacs "24.3"))

;; This program is distributed under the Eclipse Public License - v 1.0
;; For more information see https://www.eclipse.org/legal/epl-v10.html

;;; Commentary:

;; To use it, open a Python file and run M-x live-py-mode
;; If that doesn't work, put the following in your Emacs configuration file:
;;
;;   (require 'live-py-mode)

;;; Code:

;; User variables. The decision if local or not has to be left to the user.
(defvar live-py-driver nil)
(defvar live-py-dir nil)
(defvar live-py-path nil)
(defvar live-py-version nil)
(defvar live-py-lighter-delaying nil
  "Lighter during the plugin state \"delaying\".
For understanding purposes this can be set to for example \" Live-D\"
when the other `live-py-lighter-*' are adapted too.")
(defvar live-py-lighter-tracing nil
  "Lighter during the plugin state \"tracing\".
For understanding purposes this can be set to for example \" Live-T\".
when the other `live-py-lighter-*' are adapted too.")
(defvar live-py-lighter-ready " Live"
  "Lighter during the plugin state \"trace ready\".
For understanding purposes this can be set to for example \" Live-t\".
when the other `live-py-lighter-*' are adapted too.")
(defvar live-py-lighter-fail " Live-FAIL"
  "Lighter during the plugin state \"failed\".")

;; Internal variables.
(defvar live-py-output-window nil)
(defvar-local live-py-timer nil)
(defvar-local live-py-trace-buffer nil "The name of the trace buffer.")
(defvar-local live-py-module nil)
(defvar-local live-py-parent nil)
(defvar-local live-py-window-start-pos nil)
(defvar-local live-py-point-line-nr nil)
(defvar-local live-py-lighter nil)

(defun live-py-after-change-function (start stop len)
  "After a delay run the buffer through the code tracer and show trace.
START, STOP and LEN are required by `after-change-functions' but unused."
  (ignore start stop len)
  (if live-py-timer
      (cancel-timer live-py-timer)
    (when live-py-lighter-delaying
      (setq-local live-py-lighter live-py-lighter-delaying)
      ;; Here it seems not necessary to `force-mode-line-update'.
      (redisplay)))
  (setq-local live-py-timer (run-at-time 0.5 nil 'live-py-trace-update)))

(defun live-py-trace-update ()
  "Trace the Python code using code_tracer.py."
  ;; For when called from elsewhere than `live-py-after-change-function'.
  (when live-py-timer (cancel-timer live-py-timer))
  (when live-py-lighter-tracing
    (setq-local live-py-lighter live-py-lighter-tracing)
    (force-mode-line-update)
    (redisplay))
  (let* ((tracer-path (locate-file "code_tracer.py" load-path))
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
                         (and live-py-trace-buffer
                              (get-buffer live-py-trace-buffer)))))
    (save-restriction
      (widen)
      (setq-local
       live-py-lighter
       ;; Create (or recreate) if necessary, update and last but not least
       ;; display the trace buffer.
       (if (eq 0 (shell-command-on-region 1
                                          (1+ (buffer-size))
                                          command-line
                                          live-py-trace-buffer))
           live-py-lighter-ready
         live-py-lighter-fail))
      (force-mode-line-update)
      (redisplay))
    (with-current-buffer live-py-trace-buffer
      (setq-local buffer-read-only 1)
      (unless reused-buffer (toggle-truncate-lines 1)))
    (live-py-synchronize-scroll
     (line-number-at-pos (window-start)) (line-number-at-pos))
    (setq-local live-py-timer nil)))

(defun live-py-synchronize-scroll (window-start-line-nr point-line-nr)
  "Synchronize scrolling between Python buffer and trace buffer.

Pass the possibly reused (line-number-at-pos (window-start)) to
WINDOW-START-LINE-NR and (line-number-at-pos) to POINT-LINE-NR,
both are relative to (point-min). Numbering starts at 1 for all
*-LINE-NR in this function signature and body.

When the Python buffer is narrowed the trace buffer remains
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
  ;; - Repeated `next-line' towards the end of the Python buffer: The trace
  ;;   buffer suddenly lags one line behind.
  ;; See also "calculating new window-start/end without redisplay"
  ;; http://stackoverflow.com/questions/23923371 )
  (when (memq this-command '(narrow-to-region next-line viper-goto-line))
    (redisplay))
  (let ((window-start-pos (window-start))
        (point-line-nr (line-number-at-pos)))
    (unless (and (= live-py-window-start-pos window-start-pos)
                 (= live-py-point-line-nr point-line-nr))
      (setq-local live-py-window-start-pos window-start-pos)
      (setq-local live-py-point-line-nr point-line-nr)
      (if (buffer-live-p (and live-py-trace-buffer
                              (get-buffer live-py-trace-buffer)))
          (progn
            (unless (window-valid-p live-py-output-window)
              ;; Recreate output window.
              (live-py-show-output-window))
            (set-window-buffer live-py-output-window live-py-trace-buffer)
            (live-py-synchronize-scroll
             (line-number-at-pos window-start-pos) point-line-nr))
        ;; Recreate the trace buffer and if necessary the output window.
        (live-py-trace-update)))))

(defun live-py-show-output-window ()
  "Show the live-py output window."
  (delete-other-windows)
  (get-buffer-create live-py-trace-buffer)
  (toggle-truncate-lines 1)
  (with-current-buffer live-py-trace-buffer
    (toggle-truncate-lines 1)
    (setq-local show-trailing-whitespace nil))
  (setq live-py-output-window (split-window-horizontally))
  (set-window-buffer live-py-output-window live-py-trace-buffer))

(defun live-py-set-driver()
  "Prompt user to enter the driver command, with input history support.
To use a unit test, set the driver to something like this:
-m unittest mymodule.MyTest.test_method"
  (interactive)
  (setq live-py-driver (read-string "Type the driver command:"))
  (live-py-trace-update))

(defun live-py-set-version()
  "Prompt user to enter the python command, with input history support.
Typical values are 'python' or 'python3'."
  (interactive)
  (setq live-py-version (expand-file-name
                         (read-shell-command "Type the python command:")))
  (live-py-trace-update))

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
    (user-error "Working directory %s must be a parent of %s"
		live-py-dir
		buffer-file-name))
  (setq-local live-py-module (file-name-base buffer-file-name))
  (setq-local live-py-parent (directory-file-name
                              (file-name-directory buffer-file-name)))
  (while (not (string= live-py-parent live-py-dir))
    (setq-local live-py-module (concat
                                (file-name-nondirectory live-py-parent)
                                "."
                                live-py-module))
    (setq-local live-py-parent (directory-file-name
                                (file-name-directory live-py-parent))))
  (setq live-py-dir (file-name-as-directory live-py-dir))
  (live-py-trace-update)
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
    (user-error "Current buffer has no associated file"))
  (cond

   ;; Turning the mode ON.
   (live-py-mode
    (setq live-py-dir (file-name-directory buffer-file-name)
          live-py-version "python")
    ;; Create a unique name for the trace buffer.
    (setq-local live-py-trace-buffer
                (concat "*live-py-trace_"
                        (file-name-nondirectory (buffer-file-name))
                        "_"
                        (make-temp-name "")
                        "*"))
    (setq-local live-py-module (file-name-base buffer-file-name))
    (setq-local live-py-window-start-pos -1)
    (setq-local live-py-point-line-nr -1)
    (add-hook 'kill-buffer-hook 'live-py-mode-off nil t)
    (add-hook 'after-change-functions 'live-py-after-change-function nil t)
    (live-py-show-output-window)
    (live-py-trace-update)
    (add-hook 'post-command-hook 'live-py-check-to-scroll nil t))
   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions 'live-py-after-change-function t)
    (remove-hook 'post-command-hook 'live-py-check-to-scroll t)
    (remove-hook 'kill-buffer-hook 'live-py-mode-off t)
    (ignore-errors (kill-buffer live-py-trace-buffer))
    (when (window-valid-p live-py-output-window)
      (delete-window live-py-output-window))
    (toggle-truncate-lines 0))))

(provide 'live-py-mode)

;;; live-py-mode.el ends here
