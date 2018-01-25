;;; live-py-mode.el --- Live Coding in Python -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Don Kirkby

;; Author: Don Kirkby http://donkirkby.github.io
;; Keywords: live coding
;; URL: http://donkirkby.github.io/live-py-plugin/
;; Version: 2017
;; X-Original-Version: 2.20
;; Package-Requires: ((emacs "24.3"))

;; This program is distributed under the MIT License
;; For more information see https://choosealicense.com/licenses/mit/

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
(defvar live-py-args "")
(defvar live-py-update-all-delay 0.5
  "Minimum inactivity time after change in source buffer before update.
Floating point number with seconds.

Every change in the source buffer starts or restarts the timer
with this delay. As soon as it fires the trace buffer will be
updated. Set it to nil during automatic tests.")
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
(defvar-local live-py-truncate-lines-original nil)
(defvar-local live-py-source-last-active-window nil
  "Window where the source buffer was active the last time.
Can be different to the current window showing the source buffer
if the source buffer was last active in another window that still
exists and still shows the source buffer.")
(defvar-local live-py-trace-name nil)
(defvar-local live-py-timer nil)
(defvar-local live-py-module nil)
(defvar-local live-py-parent nil)
(defvar-local live-py-window-start-pos nil)
(defvar-local live-py-point-line-nr nil)
(defvar-local live-py-display-col-nr nil)
(defvar-local live-py-display-hscroll nil)
(defvar-local live-py-lighter nil)

(defun live-py-after-change-function (start stop len)
  "Start or restart timer to run `live-py-update-all'.
START, STOP and LEN are required by `after-change-functions' but unused."
  (ignore start stop len)
  (if live-py-timer
      (cancel-timer live-py-timer)
    (when live-py-lighter-delaying
      (setq-local live-py-lighter live-py-lighter-delaying)
      ;; Here it seems not necessary to `force-mode-line-update'.
      (redisplay)))
  (if live-py-update-all-delay
      (setq-local live-py-timer (run-at-time live-py-update-all-delay
                                             nil
                                             #'live-py-update-all))
    (live-py-update-all)))

(defun live-py-update-all ()
  "Update trace buffer and output window."
  ;; For when called from elsewhere than `live-py-after-change-function'.
  (when live-py-timer (cancel-timer live-py-timer))
  ;; For `live-py-lighter' in `live-py-after-change-function'.
  (setq-local live-py-timer nil)
  (when live-py-lighter-tracing
    (setq-local live-py-lighter live-py-lighter-tracing)
    (force-mode-line-update)
    (redisplay))
  (let* ((tracer-path (locate-file "code_tracer.py" load-path))
         (command-line-start (concat
			      (shell-quote-argument live-py-version)
			      " "
                  live-py-args
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
         (default-directory live-py-dir))
    ;; Don't let `shell-command-on-region' handle the window split, care
    ;; only with `live-py-create-output-window' to do it as required.
    (unless (get-buffer-window live-py-trace-name)
      (live-py-create-output-window))

    (let ((output-window (get-buffer-window live-py-trace-name)))
      (setq-local live-py-display-col-nr
                  (if output-window
                      (with-selected-window output-window
                        (current-column))
                    0))
      (setq-local live-py-display-hscroll
                  (if output-window
                      (with-selected-window output-window
                        (window-hscroll))
                    0)))
    (save-restriction
      (widen)
      (setq-local
       live-py-lighter
       ;; Update the trace buffer.
       (if (eq 0 (shell-command-on-region 1
                                          (1+ (buffer-size))
                                          command-line
                                          live-py-trace-name))
           live-py-lighter-ready
         live-py-lighter-fail))
      (force-mode-line-update)
      (redisplay))
    (live-py-update-scroll
     (line-number-at-pos (window-start)) (line-number-at-pos))))

(defun live-py-update-scroll (window-start-line-nr point-line-nr)
  "Update window start and point of trace buffer to that of source buffer.

Pass the possibly reused (line-number-at-pos (window-start)) to
WINDOW-START-LINE-NR and (line-number-at-pos) to POINT-LINE-NR,
both are relative to (point-min). Numbering starts at 1 for all
*-LINE-NR in this function signature and body.

When the source buffer is narrowed the trace buffer remains
aligned but will not hide the part after the narrowing."
  (let* ((output-window (get-buffer-window live-py-trace-name))
         (point-min-line-nr (count-lines 1 (point-min)))
         (window-start-line-nr (+ point-min-line-nr window-start-line-nr -1))
         (point-line-nr (+ point-min-line-nr point-line-nr -1))
         (display-col-nr live-py-display-col-nr)
         (display-hscroll live-py-display-hscroll))
    (unless output-window
      (live-py-create-output-window))
    (set-window-buffer output-window live-py-trace-name)
    (with-selected-window output-window
      (goto-char (point-min))
      (forward-line window-start-line-nr)
      (set-window-start output-window (point))
      (forward-line (- point-line-nr window-start-line-nr))
      (move-to-column display-col-nr t)
      (set-window-hscroll output-window display-hscroll))))

(defun live-py-post-command-function ()
  "Update window start and point of trace buffer if necessary."
  ;; Take extra care to not let this function run into a non-handled error.
  ;; On such an error the debugger will not be entered to not block Emacs
  ;; interactivity when `debug-on-error' is active, so it is easily possible
  ;; to miss the error. And on such an error the function will be removed
  ;; from `post-command-hook' which is confusing when not noticed.
  (when (memq this-command '(narrow-to-region next-line viper-goto-line))
    ;; `window-start' is for some reason not up to date after
    ;; `post-command-hook' in at least these situations:
    ;; - For a few commands like `narrow-to-region' or `viper-goto-line'.
    ;; - `next-line' towards the end of the source buffer: The trace buffer
    ;;   suddenly lags one line behind.
    ;; See also "calculating new window-start/end without redisplay"
    ;; http://stackoverflow.com/questions/23923371
    (redisplay))
  (let ((window (get-buffer-window))
        (window-start-pos (window-start))
        (point-line-nr (line-number-at-pos))
        (output-window (get-buffer-window live-py-trace-name)))
    (when (or
           ;; Are we still in the window where the source buffer was active
           ;; the last time?
           (equal live-py-source-last-active-window window)
           ;; Has the window where the source buffer was active the last
           ;; time been deleted or has it ceased to show the source buffer?
           (not (equal (window-buffer live-py-source-last-active-window)
                       (current-buffer))))
      (setq-local live-py-source-last-active-window window)
      (unless (and (= live-py-window-start-pos window-start-pos)
                   (= live-py-point-line-nr point-line-nr))
        (setq-local live-py-window-start-pos window-start-pos)
        (setq-local live-py-point-line-nr point-line-nr)
        (setq-local live-py-display-col-nr
                    (if output-window
                        (with-selected-window output-window
                          (current-column))
                      0))
        (setq-local live-py-display-hscroll
                    (if output-window
                        (with-selected-window output-window
                          (window-hscroll))
                      0))
        (if (get-buffer-window live-py-trace-name)
            (live-py-update-scroll
             (line-number-at-pos window-start-pos) point-line-nr)
          (live-py-update-all))))))

(defun live-py-create-output-window ()
  "Create the output window."
  (delete-other-windows)
  (setq-local truncate-lines t)
  (get-buffer-create live-py-trace-name)
  (with-current-buffer live-py-trace-name
    (setq-local truncate-lines t)
    (setq-local scroll-margin 0)
    (setq-local show-trailing-whitespace nil))
  (set-window-buffer (split-window-horizontally) live-py-trace-name))

(defun live-py-set-driver()
  "Prompt user to enter the driver command, with input history support.
To use a unit test, set the driver to something like this:
-m unittest mymodule.MyTest.test_method"
  (interactive)
  (setq live-py-driver (read-string "Type the driver command:"))
  (live-py-update-all))

(defun live-py-set-version()
  "Prompt user to enter the python command, with input history support.
Typical values are 'python' or 'python3'."
  (interactive)
  (setq live-py-version (executable-find
                         (read-shell-command "Type the python command:")))
  (live-py-update-all))

(defun live-py-set-args()
  "Prompt user to enter arguments for the python command, with input history support.
One possible value is '-Q new'."
  (interactive)
  (setq live-py-args (read-shell-command "Type the python arguments:"))
  (live-py-update-all))

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
  (live-py-update-all)
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


;; (defun live-py-convert-line (text)
;;   "Convert line text into a line in SVG"
;;   (let ((
;;   (format "<line stroke=\"%s\" x0=\"%d\" y0=\"%d\" x1=\"%d\" y1=\"%d\" />"
;;           fill
;;           x0 y0 x1 y1))

; x0 y0 x1 y1 fill
(defun parse-create-line ()
  (let ((rval '()))
    (dotimes (i 5)
      (forward-line)
      (setq rval (append rval (list (string-trim (thing-at-point 'line t))))))
    rval))


(defun parse-for-turtle ()
  (interactive)
  (let ((output-window (get-buffer-window live-py-trace-name)))
    (with-selected-window output-window
      (save-excursion
        (beginning-of-buffer)
        (search-forward "start_canvas")
        (point)
        (forward-line)
        (let ((current-line (thing-at-point 'word t)))
          (while (and (not (eobp)) (not (string-prefix-p current-line "end_canvas")))
            (message "AT POINT: %s" current-line)
            (if (string-prefix-p current-line "create_line")
                (message "CREATE LINE %s" (parse-create-line)))
            (forward-line)
            (setq current-line (thing-at-point 'word t)))
          (message "done loop"))))))


(defun live-py-turtle-mode ()
  "Turn on turtle mode"
  (interactive)
  (let ((output-window (get-buffer-window live-py-trace-name)))
    (with-selected-window output-window
      (insert-image (create-image "<?xml version=\"1.0\" encoding=\"utf-8\" ?><svg baseProfile=\"full\" height=\"500px\" version=\"1.1\" width=\"500px\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:ev=\"http://www.w3.org/2001/xml-events\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><defs /><rect fill=\"white\" height=\"100%\" width=\"100%\" x=\"0\" y=\"0\" /><line stroke=\"#000000\" stroke-linecap=\"round\" stroke-width=\"0\" x1=\"250.0\" x2=\"300.0\" y1=\"250.0\" y2=\"250.0\" /><line stroke=\"#000d00\" stroke-linecap=\"round\" stroke-width=\"1\" x1=\"300.0\" x2=\"258.33562479\" y1=\"250.0\" y2=\"280.270940493\" /><line stroke=\"#001a00\" stroke-linecap=\"round\" stroke-width=\"2\" x1=\"258.33562479\" x2=\"275.640576475\" y1=\"280.270940493\" y2=\"227.011775581\" /><line stroke=\"#002600\" stroke-linecap=\"round\" stroke-width=\"3\" x1=\"275.640576475\" x2=\"295.263155617\" y1=\"227.011775581\" y2=\"287.403864365\" /><line stroke=\"#003300\" stroke-linecap=\"round\" stroke-width=\"4\" x1=\"295.263155617\" x2=\"235.395898034\" y1=\"287.403864365\" y2=\"243.907755696\" /><line stroke=\"#004000\" stroke-linecap=\"round\" stroke-width=\"5\" x1=\"235.395898034\" x2=\"322.895898034\" y1=\"243.907755696\" y2=\"243.907755696\" /><line stroke=\"#004d00\" stroke-linecap=\"round\" stroke-width=\"6\" x1=\"322.895898034\" x2=\"238.758130619\" y1=\"243.907755696\" y2=\"305.037421934\" /><line stroke=\"#005900\" stroke-linecap=\"round\" stroke-width=\"7\" x1=\"238.758130619\" x2=\"276.921729424\" y1=\"305.037421934\" y2=\"187.581942172\" /><line stroke=\"#006600\" stroke-linecap=\"round\" stroke-width=\"8\" x1=\"276.921729424\" x2=\"322.038210603\" y1=\"187.581942172\" y2=\"326.436193551\" /><line stroke=\"#007300\" stroke-linecap=\"round\" stroke-width=\"9\" x1=\"322.038210603\" x2=\"183.291796068\" y1=\"326.436193551\" y2=\"225.631022783\" /><line stroke=\"#008000\" stroke-linecap=\"round\" stroke-width=\"10\" x1=\"183.291796068\" x2=\"383.291796068\" y1=\"225.631022783\" y2=\"225.631022783\" /><line stroke=\"#008c00\" stroke-linecap=\"round\" stroke-width=\"11\" x1=\"383.291796068\" x2=\"196.00436187\" y1=\"225.631022783\" y2=\"361.703308688\" /><line stroke=\"#009900\" stroke-linecap=\"round\" stroke-width=\"12\" x1=\"196.00436187\" x2=\"278.202882373\" y1=\"361.703308688\" y2=\"108.722275354\" /><line stroke=\"#00a600\" stroke-linecap=\"round\" stroke-width=\"13\" x1=\"278.202882373\" x2=\"371.989540166\" y1=\"108.722275354\" y2=\"397.367928049\" /><line stroke=\"#00b300\" stroke-linecap=\"round\" stroke-width=\"14\" x1=\"371.989540166\" x2=\"93.6876941013\" y1=\"397.367928049\" y2=\"195.169801261\" /><line stroke=\"#00bf00\" stroke-linecap=\"round\" stroke-width=\"15\" x1=\"93.6876941013\" x2=\"481.187694101\" y1=\"195.169801261\" y2=\"195.169801261\" /><line stroke=\"#00cc00\" stroke-linecap=\"round\" stroke-width=\"16\" x1=\"481.187694101\" x2=\"130.074318543\" y1=\"195.169801261\" y2=\"450.268600756\" /><line stroke=\"#00d900\" stroke-linecap=\"round\" stroke-width=\"17\" x1=\"130.074318543\" x2=\"279.484035323\" y1=\"450.268600756\" y2=\"-9.56722487308\" /><line stroke=\"#00e600\" stroke-linecap=\"round\" stroke-width=\"18\" x1=\"279.484035323\" x2=\"445.117144308\" y1=\"-9.56722487308\" y2=\"500.199067861\" /><line stroke=\"#00f200\" stroke-linecap=\"round\" stroke-width=\"19\" x1=\"445.117144308\" x2=\"-33.416407865\" y1=\"500.199067861\" y2=\"152.52409113\" /><polygon fill=\"#0000ff\" fill-rule=\"evenodd\" points=\"250.0,250.0 300.0,250.0 258.33562479,280.270940493 275.640576475,227.011775581 295.263155617,287.403864365 235.395898034,243.907755696 322.895898034,243.907755696 238.758130619,305.037421934 276.921729424,187.581942172 322.038210603,326.436193551 183.291796068,225.631022783 383.291796068,225.631022783 196.00436187,361.703308688 278.202882373,108.722275354 371.989540166,397.367928049 93.6876941013,195.169801261 481.187694101,195.169801261 130.074318543,450.268600756 279.484035323,-9.56722487308 445.117144308,500.199067861 -33.416407865,152.52409113\" /><line stroke=\"#000000\" stroke-linecap=\"round\" stroke-width=\"0\" x1=\"250.0\" x2=\"300.0\" y1=\"250.0\" y2=\"250.0\" /><line stroke=\"#000d00\" stroke-linecap=\"round\" stroke-width=\"1\" x1=\"300.0\" x2=\"258.33562479\" y1=\"250.0\" y2=\"280.270940493\" /><line stroke=\"#001a00\" stroke-linecap=\"round\" stroke-width=\"2\" x1=\"258.33562479\" x2=\"275.640576475\" y1=\"280.270940493\" y2=\"227.011775581\" /><line stroke=\"#002600\" stroke-linecap=\"round\" stroke-width=\"3\" x1=\"275.640576475\" x2=\"295.263155617\" y1=\"227.011775581\" y2=\"287.403864365\" /><line stroke=\"#003300\" stroke-linecap=\"round\" stroke-width=\"4\" x1=\"295.263155617\" x2=\"235.395898034\" y1=\"287.403864365\" y2=\"243.907755696\" /><line stroke=\"#004000\" stroke-linecap=\"round\" stroke-width=\"5\" x1=\"235.395898034\" x2=\"322.895898034\" y1=\"243.907755696\" y2=\"243.907755696\" /><line stroke=\"#004d00\" stroke-linecap=\"round\" stroke-width=\"6\" x1=\"322.895898034\" x2=\"238.758130619\" y1=\"243.907755696\" y2=\"305.037421934\" /><line stroke=\"#005900\" stroke-linecap=\"round\" stroke-width=\"7\" x1=\"238.758130619\" x2=\"276.921729424\" y1=\"305.037421934\" y2=\"187.581942172\" /><line stroke=\"#006600\" stroke-linecap=\"round\" stroke-width=\"8\" x1=\"276.921729424\" x2=\"322.038210603\" y1=\"187.581942172\" y2=\"326.436193551\" /><line stroke=\"#007300\" stroke-linecap=\"round\" stroke-width=\"9\" x1=\"322.038210603\" x2=\"183.291796068\" y1=\"326.436193551\" y2=\"225.631022783\" /><line stroke=\"#008000\" stroke-linecap=\"round\" stroke-width=\"10\" x1=\"183.291796068\" x2=\"383.291796068\" y1=\"225.631022783\" y2=\"225.631022783\" /><line stroke=\"#008c00\" stroke-linecap=\"round\" stroke-width=\"11\" x1=\"383.291796068\" x2=\"196.00436187\" y1=\"225.631022783\" y2=\"361.703308688\" /><line stroke=\"#009900\" stroke-linecap=\"round\" stroke-width=\"12\" x1=\"196.00436187\" x2=\"278.202882373\" y1=\"361.703308688\" y2=\"108.722275354\" /><line stroke=\"#00a600\" stroke-linecap=\"round\" stroke-width=\"13\" x1=\"278.202882373\" x2=\"371.989540166\" y1=\"108.722275354\" y2=\"397.367928049\" /><line stroke=\"#00b300\" stroke-linecap=\"round\" stroke-width=\"14\" x1=\"371.989540166\" x2=\"93.6876941013\" y1=\"397.367928049\" y2=\"195.169801261\" /><line stroke=\"#00bf00\" stroke-linecap=\"round\" stroke-width=\"15\" x1=\"93.6876941013\" x2=\"481.187694101\" y1=\"195.169801261\" y2=\"195.169801261\" /><line stroke=\"#00cc00\" stroke-linecap=\"round\" stroke-width=\"16\" x1=\"481.187694101\" x2=\"130.074318543\" y1=\"195.169801261\" y2=\"450.268600756\" /><line stroke=\"#00d900\" stroke-linecap=\"round\" stroke-width=\"17\" x1=\"130.074318543\" x2=\"279.484035323\" y1=\"450.268600756\" y2=\"-9.56722487308\" /><line stroke=\"#00e600\" stroke-linecap=\"round\" stroke-width=\"18\" x1=\"279.484035323\" x2=\"445.117144308\" y1=\"-9.56722487308\" y2=\"500.199067861\" /><line stroke=\"#00f200\" stroke-linecap=\"round\" stroke-width=\"19\" x1=\"445.117144308\" x2=\"-33.416407865\" y1=\"500.199067861\" y2=\"152.52409113\" /></svg>" 'svg 't)))))


;;;###autoload
(define-minor-mode live-py-mode
  "Minor mode to do on-the-fly Python tracing.
When called interactively, toggles the minor mode.
With arg, turn mode on if and only if arg is positive.

\\{live-py-mode-map}"
  :group 'live-py-mode
  :lighter live-py-lighter
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c M-d") #'live-py-set-driver)
            (define-key map (kbd "C-c M-w") #'live-py-set-dir)
            (define-key map (kbd "C-c M-p") #'live-py-set-path)
            (define-key map (kbd "C-c M-v") #'live-py-set-version)
            (define-key map (kbd "C-c M-a") #'live-py-set-args)
            map)
  (unless (buffer-file-name)
    (user-error "Current buffer has no associated file"))
  (cond
   ;; Turning the mode ON.
   (live-py-mode
    ;; User variables.
    (setq live-py-dir (file-name-directory buffer-file-name)
          live-py-version "python")

    ;; Internal variables.
    (setq-local live-py-truncate-lines-original truncate-lines)
    (setq-local live-py-source-last-active-window (get-buffer-window))
    ;; Create a unique name for the trace buffer.
    (setq-local live-py-trace-name
                (concat "*live-py-trace_"
                        (file-name-nondirectory (buffer-file-name))
                        "_"
                        (make-temp-name "")
                        "*"))
    (setq-local live-py-module (file-name-base buffer-file-name))
    (setq-local live-py-timer nil)
    (setq-local live-py-window-start-pos -1)
    (setq-local live-py-point-line-nr -1)
    (setq-local live-py-lighter live-py-lighter-fail)

    (add-hook 'kill-buffer-hook #'live-py-mode-off nil t)
    (add-hook 'after-change-functions #'live-py-after-change-function nil t)
    (add-hook 'post-command-hook #'live-py-post-command-function nil t)
    (live-py-update-all))
   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions #'live-py-after-change-function t)
    (remove-hook 'post-command-hook #'live-py-post-command-function t)
    (remove-hook 'kill-buffer-hook #'live-py-mode-off t)
    (let ((output-window (get-buffer-window live-py-trace-name)))
      (when output-window (delete-window output-window)))
    (ignore-errors (kill-buffer live-py-trace-name))
    (setq-local truncate-lines live-py-truncate-lines-original))))

(provide 'live-py-mode)

;; Local Variables:
;;   coding: us-ascii-unix
;;   fill-column: 76
;;   indent-tabs-mode: nil
;; End:

;;; live-py-mode.el ends here
