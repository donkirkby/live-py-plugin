;;; live-py-test.el --- Emacs regression tests -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Michael Brand

;; Author: Michael Brand <michael.ch.brand@gmail.com>
;; Keywords: live coding
;; URL: http://donkirkby.github.io/live-py-plugin/
;; Version: 2017
;; Package-Requires: ((emacs "24.3"))

;; This program is distributed under the Eclipse Public License - v 1.0
;; For more information see https://www.eclipse.org/legal/epl-v10.html

;;; Commentary:

;; To run all Emacs regression tests (ERT) in a shell:
;; emacs -Q -nw \
;;     -L emacs-live-py-mode \
;;     -L plugin/PySrc \
;;     -l live-py-mode.el \
;;     -l live-py-test.el \
;;     -f ert-run-tests-batch-and-exit
;; and check that the exit status is 0.
;;
;; To run all ERT interactively:
;; (progn (ert-delete-all-tests) (eval-buffer) (ert t))
;; In the ERT buffer type R to rerun all these tests, r to rerun one test.
;; When a test fails move there and type l to get a list of all `should',
;; including those hidden in `live-py-test-with-temp-text-in-file' and
;; function calls, to count to the failed one. If you use an Emacs package
;; for Python development like for example elpy remember to disable it
;; before running ERT.
;;
;; It is highly recommended to use simply "C-c C-c" when running just the
;; ERT of the `ert-deftest' form where the point is currently in with
;; something like:
;; (defun f-eval-defun-and-ert ()
;;   "Evaluate/run the top-level form containing point, or after point."
;;   (interactive)
;;   (ert-delete-all-tests)
;;   (let ((def (call-interactively #'eval-defun)))
;;     (cond ((ert-test-boundp def)
;;            (ert def)
;;            (other-window 1))
;;           (t
;;            (message "INF: No action defined after `eval-defun'")))))
;; (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'f-eval-defun-and-ert)
;;
;; The ordering of the arguments in `equal' etc. follows "Unit testing: why
;; is the expected argument always first in equality tests?":
;; http://stackoverflow.com/questions/9331259
;;
;; The `ert-deftest' are grouped hierarchically by path-like names and
;; prefix-numbered from top to bottom to keep the same order in the test
;; list just for convenience, execution order does not have a dependency
;; among the tests. The convenience is when more than one test fails: It
;; makes sense to work first on that one that is the culprit, not the
;; depending symptoms. The prefix numbers try to reflect this where more
;; obvious.
;;
;; Functions are used instead of key sequences for user interaction in the
;; tests to be independent of the interactive user's key bindings like for
;; example from Viper mode.

;;; Code:

(require 'cl-extra)
(require 'ert)

;;; The framework for the testing

(defmacro live-py-test-with-temp-text-in-file (text &rest body)
  "Make temporary file buffer with TEXT in live-py mode and run BODY.
Within BODY the variable value FILE can be passed to
`live-py-test-buf' as the source buffer name."
  (declare (indent 1))
  `(let* ((path (make-temp-file "emacs-ert_" nil ".py"))
          (file (file-name-nondirectory path)))
     (delete-other-windows)
     (with-temp-file path (insert ,text))
     (find-file path)
     (setq-local truncate-lines nil)
     (live-py-mode)
     (setq-local live-py-update-all-delay nil)
     ;; Rule out a failure during setting up the initial test environment.
     (should (equal '(:s 1 1 :t 1 1) (live-py-test-buf file)))
     ;; Change window start line and point line to 2 and 3, it keeps the
     ;; chance for a false test success lower than with 1 and 1.
     (live-py-test-scroll-line)
     (live-py-test-next-line)
     ;; Rule out again before running the tests.
     (should (equal '(:s 2 3 :t 2 3) (live-py-test-buf file)))
     ;; Run the tests.
     ,@body
     ;; Succeeded, cleanup.
     (live-py-mode 0)
     (should (equal '(nil) (live-py-test-truncate-lines)))
     (kill-buffer file)
     (delete-file path)))
(def-edebug-spec live-py-test-with-temp-text-in-file (form body))

(defun live-py-test-buf (file)
  "Return a list with some buffer properties for all windows.
FILE is the source buffer name. The buffer propierties are a tag
and the line numbers of window start and point. The tag is :s for
the source buffer, :t for the trace buffer and :- for unknown."
  (cl-mapcan
   (lambda (window)
     (with-selected-window window
       (list (cond ((string= file
                             ;; For example emacs-ert_12716U-z.py
                             (buffer-name))
                    :s)
                   ((string-match
                     (concat "^\\*live-py-trace_"
                             (regexp-quote file)
                             "_[-_0-9A-Za-z]+\\*$")
                     ;; For example
                     ;; *live-py-trace_emacs-ert_12716U-z.py_12716GID*
                     (buffer-name))
                    :t)
                   (t
                    :-))
             (line-number-at-pos (window-start))
             (line-number-at-pos))))
   (window-list)))

(defun live-py-test-truncate-lines ()
  "Return a list with the buffer's `truncate-lines' for all windows."
  (mapcar (lambda (window) (with-selected-window window
                             truncate-lines))
          (window-list)))

(defun live-py-test-edit ()
  "Edit buffer and undo, simulated.
Used to test the update after an edit."
  ;; Trigger the update with nothing more than `after-change-functions' with
  ;; dummy argument values.
  (mapc (lambda (func) (when (functionp func) (funcall func 1 1 0)))
        after-change-functions))

(defun live-py-test-forward-char (&optional chars)
  "Move point forward CHARS chars.
Used to test the not update after moving point."
  (forward-char chars)
  ;; Trigger the update with nothing more than `post-command-hook'.
  (run-hooks 'post-command-hook))

(defun live-py-test-next-line (&optional lines)
  "Move point down LINES lines.
Used to test the update after moving point."
  (next-line lines)
  ;; Trigger the update with nothing more than `post-command-hook'.
  (run-hooks 'post-command-hook))

(defun live-py-test-scroll-line (&optional lines)
  "Scroll up LINES lines.
Used to test the update after scrolling."
  (scroll-up (or lines 1))
  ;; Trigger the update with nothing more than `post-command-hook'.
  (run-hooks 'post-command-hook))

(defconst live-py-test-source
  "x = 1 * 10
x = 2 * 10
x = 3 * 10
x = 4 * 10
x = 5 * 10
# Suppress the message about guessing python-indent-offset.
if None:
    None
")

(defconst live-py-test-trace
  "x = 10 
x = 20 
x = 30 
x = 40 
x = 50 
")

;;; Test the use cases

(ert-deftest live-py-test/1-basic/1-initial-state ()
  "The trace buffer shows the trace output from code_tracer.py"
  (live-py-test-with-temp-text-in-file live-py-test-source
    (should (equal '(t t) (live-py-test-truncate-lines)))
    (with-selected-window (get-buffer-window live-py-trace-name)
      (should (string= live-py-test-trace (buffer-string))))))

(ert-deftest live-py-test/1-basic/2-move-or-scroll ()
  "Move point or scroll in the source buffer."
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test-next-line)
    (should (equal '(:s 2 4 :t 2 4) (live-py-test-buf file)))
    (live-py-test-scroll-line)
    (should (equal '(:s 3 4 :t 3 4) (live-py-test-buf file)))
    (live-py-test-scroll-line)
    (should (equal '(:s 4 4 :t 4 4) (live-py-test-buf file)))))

(ert-deftest live-py-test/1-basic/3-narrow-to-region ()
  "Narrowing the source must at least compensate the trace alignment."
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test-next-line -1)
    (should (equal '(:s 2 2 :t 2 2) (live-py-test-buf file)))
    (narrow-to-region (point) (save-excursion (forward-line 3) (point)))
    (live-py-test-scroll-line)
    (live-py-test-next-line)
    ;; Change these when narrowing is transferred to the trace buffer.
    (should (equal '(:s 2 3 :t 3 4) (live-py-test-buf file)))
    (with-selected-window (get-buffer-window live-py-trace-name)
      (should (string= live-py-test-trace (buffer-string))))))

;; Recreate the killed trace buffer.
(defun live-py-test/2-recreate/1-buffer (file)
  (kill-buffer live-py-trace-name)
  (should (equal '(:s 2 3 :s 1 1) (live-py-test-buf file))))
(ert-deftest live-py-test/2-recreate/1-buffer/edit ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test/2-recreate/1-buffer file)
    (live-py-test-edit)
    (should (equal '(:s 2 3 :t 2 3) (live-py-test-buf file)))))
(ert-deftest live-py-test/2-recreate/1-buffer/move ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test/2-recreate/1-buffer file)
    (live-py-test-forward-char 5)
    (should (equal '(:s 2 3 :s 1 1) (live-py-test-buf file)))
    (live-py-test-next-line)
    (should (equal '(:s 2 4 :t 2 4) (live-py-test-buf file)))))

;; Recreate the deleted output window.
(defun live-py-test/2-recreate/2-window (file)
  (delete-other-windows)
  (should (equal '(:s 2 3) (live-py-test-buf file))))
(ert-deftest live-py-test/2-recreate/2-window/edit ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test/2-recreate/2-window file)
    (live-py-test-edit)
    (should (equal '(:s 2 3 :t 2 3) (live-py-test-buf file)))))
(ert-deftest live-py-test/2-recreate/2-window/move ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test/2-recreate/2-window file)
    (live-py-test-forward-char 5)
    (should (equal '(:s 2 3) (live-py-test-buf file)))
    (live-py-test-next-line)
    (should (equal '(:s 2 4 :t 2 4) (live-py-test-buf file)))))

;; Recreate the killed trace buffer and the deleted output window.
(defun live-py-test/2-recreate/3-buffer-and-window (file)
  (kill-buffer live-py-trace-name)
  (delete-other-windows)
  (should (equal '(:s 2 3) (live-py-test-buf file))))
(ert-deftest live-py-test/2-recreate/3-buffer-and-window/edit ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test/2-recreate/3-buffer-and-window file)
    (live-py-test-edit)
    (should (equal '(:s 2 3 :t 2 3) (live-py-test-buf file)))))
(ert-deftest live-py-test/2-recreate/3-buffer-and-window/move ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test/2-recreate/3-buffer-and-window file)
    (live-py-test-forward-char 5)
    (should (equal '(:s 2 3) (live-py-test-buf file)))
    (live-py-test-next-line)
    (should (equal '(:s 2 4 :t 2 4) (live-py-test-buf file)))))

;; Keep source in first window, browse something else in the other window
;; and come back. http://github.com/donkirkby/live-py-plugin/issues/100
(defun live-py-test/3-other-window/1-browse-else (file)
  (let ((temp-buffer (generate-new-buffer "*temp*")))
    (other-window 1)
    (switch-to-buffer temp-buffer)
    (insert "a\nb\nc\nd\ne\nf\ng\nh\n")
    (live-py-test-scroll-line 6)
    (other-window 1)
    (should (equal '(:s 2 3 :- 7 9) (live-py-test-buf file)))
    temp-buffer))
(ert-deftest live-py-test/3-other-window/1-browse-else/edit ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (let ((temp-buffer (live-py-test/3-other-window/1-browse-else file)))
      (live-py-test-edit)
      (should (equal '(:s 2 3 :t 2 3) (live-py-test-buf file)))
      (kill-buffer temp-buffer))))
(ert-deftest live-py-test/3-other-window/1-browse-else/move ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (let ((temp-buffer (live-py-test/3-other-window/1-browse-else file)))
      (live-py-test-forward-char 5)
      (should (equal '(:s 2 3 :- 7 9) (live-py-test-buf file)))
      (live-py-test-next-line)
      (should (equal '(:s 2 4 :t 2 4) (live-py-test-buf file)))
      (kill-buffer temp-buffer))))

;; Keep source in first window, browse it in the other window and come back.
;; The other window must not trigger any update as long as the first window
;; still shows the source buffer.
(defun live-py-test/3-other-window/2-browse-source (file)
  (other-window 1)
  (switch-to-buffer file)
  (live-py-test-scroll-line 2)
  (live-py-test-next-line 2)
  (other-window 1)
  (should (equal '(:s 2 3 :s 3 5) (live-py-test-buf file))))
(ert-deftest live-py-test/3-other-window/2-browse-source/edit ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test/3-other-window/2-browse-source file)
    (live-py-test-edit)
    (should (equal '(:s 2 3 :t 2 3) (live-py-test-buf file)))))
(ert-deftest live-py-test/3-other-window/2-browse-source/move ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test/3-other-window/2-browse-source file)
    (live-py-test-forward-char 5)
    (should (equal '(:s 2 3 :s 3 5) (live-py-test-buf file)))
    (live-py-test-next-line)
    (should (equal '(:s 2 4 :t 2 4) (live-py-test-buf file)))))

(ert-deftest live-py-test/3-other-window/3-keep-source-and-swap ()
  "Keep source in first window and edit it in the other window.
The edit must update the trace buffer as otherwise it will be
outdated in a later scroll sync."
  (live-py-test-with-temp-text-in-file live-py-test-source
    (other-window 1)
    (switch-to-buffer file)
    (live-py-test-scroll-line)
    (live-py-test-next-line)
    (should (equal '(:s 2 4 :s 2 3) (live-py-test-buf file)))
    (live-py-test-edit)
    (should (equal '(:s 2 4 :t 2 4) (live-py-test-buf file)))))

;; Abandon source in first window and swap it to the other window. When the
;; first window does not show the source buffer any more the other window
;; can take it over. Furthermore, respect a user's change of truncate-lines
;; in both buffers.
(defun live-py-test/3-other-window/4-abandon-source-and-swap (file)
  (switch-to-buffer live-py-trace-name)
  (scroll-up 1)
  (other-window 1)
  (scroll-up 1)
  (next-line)
  (setq-local truncate-lines nil)
  (should (equal '(:t 3 4 :t 3 3) (live-py-test-buf file)))
  (switch-to-buffer file)
  (setq-local truncate-lines nil)
  (should (equal '(:s 2 3 :t 3 3) (live-py-test-buf file))))
(ert-deftest live-py-test/3-other-window/4-abandon-source-and-swap/edit ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test/3-other-window/4-abandon-source-and-swap file)
    (live-py-test-edit)
    (should (equal '(:s 2 3 :t 2 3) (live-py-test-buf file)))
    (should (equal '(nil nil) (live-py-test-truncate-lines)))))
(ert-deftest live-py-test/3-other-window/4-abandon-source-and-swap/move ()
  (live-py-test-with-temp-text-in-file live-py-test-source
    (live-py-test/3-other-window/4-abandon-source-and-swap file)
    (live-py-test-forward-char 5)
    (should (equal '(:s 2 3 :t 3 3) (live-py-test-buf file)))
    (live-py-test-next-line)
    (should (equal '(:s 2 4 :t 2 4) (live-py-test-buf file)))
    (should (equal '(nil nil) (live-py-test-truncate-lines)))))

;; Local Variables:
;;   coding: us-ascii-unix
;;   fill-column: 76
;;   indent-tabs-mode: nil
;; End:

;;; live-py-test.el ends here
