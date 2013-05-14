;;; log4e.el --- provide logging framework for elisp

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: log
;; URL: https://github.com/aki2o/log4e
;; Version: 0.2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension provides logging framework for elisp.

;;; Dependency:
;; 
;; Nothing.

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your elisp file.
;; 
;; (require 'log4e)

;;; Configuration:
;; 
;; See <https://github.com/aki2o/log4e/blob/master/README.md>
;; Otherwise, eval following sexp.
;; (describe-function 'log4e:deflogger)

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "log4e:" :docstring t)
;; `log4e:next-log'
;; Move to start of next log on log4e-mode.
;; `log4e:previous-log'
;; Move to start of previous log on log4e-mode.
;; `log4e:insert-start-log-quickly'
;; Insert logging statment for trace level log at start of current function/macro.
;; 
;;  *** END auto-documentation
;; 
;; For detail, see <https://github.com/aki2o/log4e/blob/master/README.md>
;; 
;; [Note] Other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK


;; Enjoy!!!


(eval-when-compile (require 'cl))


(defconst log4e-log-level-alist '((fatal . 6)
                                  (error . 5)
                                  (warn  . 4)
                                  (info  . 3)
                                  (debug . 2)
                                  (trace . 1))
  "Alist of log level value")

(defconst log4e-default-logging-function-name-alist '((fatal . "log-fatal")
                                                      (error . "log-error")
                                                      (warn  . "log-warn")
                                                      (info  . "log-info")
                                                      (debug . "log-debug")
                                                      (trace . "log-trace"))
  "Alist of logging function name at default")


(defmacro* log4e:deflogger (prefix msgtmpl timetmpl &optional log-function-name-custom-alist)
  "Define the functions of logging for your elisp.

Specification:
 After eval this, you can use the functions for supporting about logging. They are the following ...
 - do logging for each log level. Log level are trace, debug, info, warn, error and fatal.
 - set max and min log level.
 - switch logging.
 - switch debugging.
 - open and clear log buffer.
 - send bug report for you.
 For details, see Functions section.

Argument:
 - PREFIX is string as your elisp prefix.
 - MSGTMPL is string as format of log. The following words has a special meaning.
   - %t ... Replaced with time string. About it, see TIMETMPL argument.
   - %l ... Replaced with log level. They are 'TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'.
   - %m ... Replaced with log message that passed by you.
 - TIMETMPL is string as format of time. This value is passed to `format-time-string'.
 - LOG-FUNCTION-NAME-CUSTOM-ALIST is alist as the function name of logging.
   - If this value is nil, define the following functions.
      yourprefix--log-trace
      yourprefix--log-debug
      ...
      yourprefix--log-fatal
   - If you want to custom the name of them, give like the following value.
      '((fatal . \"fatal\")
        (error . \"error\")
        (warn  . \"warn\")
        (info  . \"info\")
        (debug . \"debug\")
        (trace . \"trace\"))
     Then, define the following functions.
      yourprefix--trace
      yourprefix--debug
      ...
      yourprefix--fatal

Functions:
 List all functions defined below. PREFIX is your prefix.
 - PREFIX--log-fatal   ... #1
 - PREFIX--log-error   ... #1
 - PREFIX--log-warn    ... #1
 - PREFIX--log-info    ... #1
 - PREFIX--log-debug   ... #1
 - PREFIX--log-trace   ... #1
 - PREFIX--log
 - PREFIX--log-set-level
 - PREFIX--log-enable-logging            ... #2
 - PREFIX--log-disable-logging           ... #2
 - PREFIX--log-enable-debugging          ... #2
 - PREFIX--log-disable-debugging         ... #2
 - PREFIX--log-debugging-p
 - PREFIX--log-set-coding-system
 - PREFIX--log-set-author-mail-address
 - PREFIX--log-clear-log                 ... #2
 - PREFIX--log-open-log                  ... #2
 - PREFIX--log-open-log-if-debug

 #1 : You can customize this name
 #2 : This is command

Example:
;; If you develop elisp that has prefix \"hoge\", write and eval the following sexp in your elisp file.

 (require 'log4e)
 (log4e:deflogger \"hoge\" \"%t [%l] %m\" \"%H:%M:%S\")

;; Eval the following
 (hoge--log-enable-logging)

;; Then, write the following

 (defun hoge-do-hoge (hoge)
   (if (not (stringp hoge))
       (hoge--log-fatal \"failed do hoge : hoge is '%s'\" hoge)
     (hoge--log-debug \"start do hoge about '%s'\" hoge)
     (message \"hoge!\")
     (hoge--log-info \"done hoge about '%s'\" hoge)))

;; Eval the following
 (hoge-do-hoge \"HOGEGE\")

;; Do M-x hoge--log-open-log
;; Open the buffer which name is \" *log4e-hoge*\". The buffer string is below
12:34:56 [INFO ] done hoge about 'HOGEGE'

;; Eval the following
 (hoge--log-set-level 'trace)
 (hoge-do-hoge \"FUGAGA\")

;; Do M-x hoge--log-open-log
;; Open the buffer. its string is below
12:34:56 [INFO ] done hoge about 'HOGEGE'
12:35:43 [DEBUG] start do hoge about 'FUGAGA'
12:35:43 [INFO ] done hoge about 'FUGAGA'
 
"
  (declare (indent 0))
  (if (or (not (stringp prefix))
          (string= prefix "")
          (not (stringp msgtmpl))
          (string= msgtmpl "")
          (not (stringp timetmpl))
          (string= timetmpl ""))
      (message "[LOG4E] invalid argument of deflogger")
    (let* ((buffsym (intern (concat "log4e--log-buffer-" prefix)))
           (msgtmplsym (intern (concat "log4e--log-templete-" prefix)))
           (timetmplsym (intern (concat "log4e--time-templete-" prefix)))
           (minlvlsym (intern (concat "log4e--min-level-" prefix)))
           (maxlvlsym (intern (concat "log4e--max-level-" prefix)))
           (tglsym (intern (concat "log4e--toggle-logging-" prefix)))
           (dbgsym (intern (concat "log4e--toggle-debugging-" prefix)))
           (codesym (intern (concat "log4e--buffer-coding-system-" prefix)))
           (addrsym (intern (concat "log4e--author-mail-address-" prefix)))
           (funcnm-alist (loop with custom-alist = (car (cdr log-function-name-custom-alist))
                               for e in (list 'fatal 'error 'warn 'info 'debug 'trace)
                               collect (or (assq e custom-alist)
                                           (assq e log4e-default-logging-function-name-alist)))))
      `(progn
         (defvar ,buffsym (format " *log4e-%s*" ,prefix))
         (defvar ,msgtmplsym ,msgtmpl)
         (defvar ,timetmplsym ,timetmpl)
         (defvar ,minlvlsym 'info)
         (defvar ,maxlvlsym 'fatal)
         (defvar ,tglsym nil)
         (defvar ,dbgsym nil)
         (defvar ,codesym nil)
         (defvar ,addrsym nil)
         (defun ,(intern (concat prefix "--log-set-level")) (minlevel &optional maxlevel)
           "Set range for doing logging.

MINLEVEL is symbol of lowest level for doing logging. its default is 'info.
MAXLEVEL is symbol of highest level for doing logging. its default is 'fatal."
           (setq ,minlvlsym minlevel)
           (setq ,maxlvlsym maxlevel))
         (defun ,(intern (concat prefix "--log-enable-logging")) ()
           "Enable logging by logging functions."
           (interactive)
           (setq ,tglsym t))
         (defun ,(intern (concat prefix "--log-disable-logging")) ()
           "Disable logging by logging functions."
           (interactive)
           (setq ,tglsym nil))
         (defun ,(intern (concat prefix "--log-enable-debugging")) ()
           "Enable debugging and logging.

`PREFIX--log-debugging-p' will return t."
           (interactive)
           (setq ,tglsym t)
           (setq ,dbgsym t))
         (defun ,(intern (concat prefix "--log-disable-debugging")) ()
           "Disable debugging.

`PREFIX--log-debugging-p' will return nil."
           (interactive)
           (setq ,dbgsym nil))
         (defun ,(intern (concat prefix "--log-debugging-p")) ()
           ,dbgsym)
         (defun ,(intern (concat prefix "--log-set-coding-system")) (coding-system)
           "Set charset and linefeed of LOG-BUFFER.

CODING-SYSTEM is symbol for setting to `buffer-file-coding-system'.
LOG-BUFFER is a buffer which name is \" *log4e-PREFIX*\"."
           (setq ,codesym coding-system))
;;          (defun ,(intern (concat prefix "--log-set-author-mail-address")) (before-atmark after-atmark)
;;            "Set mail address of author for elisp that has PREFIX. This value is used SEND-REPORT.

;; BEFORE-ATMARK is string as part of mail address. If your address is \"hoge@example.co.jp\", it is \"hoge\".
;; AFTER-ATMARK is string as part of mail address. If your address is \"hoge@example.co.jp\", it is \"example.co.jp\".
;; SEND-REPORT is `PREFIX--log-send-report-if-not-debug'."
;;            (setq ,addrsym (concat before-atmark "@" after-atmark)))
         (defun ,(intern (concat prefix "--log")) (level msg &rest msgargs)
           "Do logging for any level log.

LEVEL is symbol of log level. it is member of '(trace debug info warn error fatal).
MSG is log text. About its format, see `log4e:deflogger'.
MSGARGS is anything. They are expand in MSG as string."
           (apply 'log4e--logging ,buffsym ,codesym ,msgtmplsym ,timetmplsym ,minlvlsym ,maxlvlsym ,tglsym level msg msgargs))
         (defun ,(intern (concat prefix "--" (assoc-default 'fatal funcnm-alist))) (msg &rest msgargs)
           "Do logging for fatal level log.

MSG is log text. About its format, see `log4e:deflogger'.
MSGARGS is anything. They are expand in MSG as string."
           (apply 'log4e--logging ,buffsym ,codesym ,msgtmplsym ,timetmplsym ,minlvlsym ,maxlvlsym ,tglsym 'fatal msg msgargs))
         (defun ,(intern (concat prefix "--" (assoc-default 'error funcnm-alist))) (msg &rest msgargs)
           "Do logging for error level log.

MSG is log text. About its format, see `log4e:deflogger'.
MSGARGS is anything. They are expand in MSG as string."
           (apply 'log4e--logging ,buffsym ,codesym ,msgtmplsym ,timetmplsym ,minlvlsym ,maxlvlsym ,tglsym 'error msg msgargs))
         (defun ,(intern (concat prefix "--" (assoc-default 'warn funcnm-alist))) (msg &rest msgargs)
           "Do logging for warning level log.

MSG is log text. About its format, see `log4e:deflogger'.
MSGARGS is anything. They are expand in MSG as string."
           (apply 'log4e--logging ,buffsym ,codesym ,msgtmplsym ,timetmplsym ,minlvlsym ,maxlvlsym ,tglsym 'warn msg msgargs))
         (defun ,(intern (concat prefix "--" (assoc-default 'info funcnm-alist))) (msg &rest msgargs)
           "Do logging for infomation level log.

MSG is log text. About its format, see `log4e:deflogger'.
MSGARGS is anything. They are expand in MSG as string."
           (apply 'log4e--logging ,buffsym ,codesym ,msgtmplsym ,timetmplsym ,minlvlsym ,maxlvlsym ,tglsym 'info msg msgargs))
         (defun ,(intern (concat prefix "--" (assoc-default 'debug funcnm-alist))) (msg &rest msgargs)
           "Do logging for debug level log.

MSG is log text. About its format, see `log4e:deflogger'.
MSGARGS is anything. They are expand in MSG as string."
           (apply 'log4e--logging ,buffsym ,codesym ,msgtmplsym ,timetmplsym ,minlvlsym ,maxlvlsym ,tglsym 'debug msg msgargs))
         (defun ,(intern (concat prefix "--" (assoc-default 'trace funcnm-alist))) (msg &rest msgargs)
           "Do logging for trace level log.

MSG is log text. About its format, see `log4e:deflogger'.
MSGARGS is anything. They are expand in MSG as string."
           (apply 'log4e--logging ,buffsym ,codesym ,msgtmplsym ,timetmplsym ,minlvlsym ,maxlvlsym ,tglsym 'trace msg msgargs))
         (defun ,(intern (concat prefix "--log-clear-log")) ()
           "Clear buffer string of buffer which name is \" *log4e-PREFIX*\"."
           (interactive)
           (log4e--clear-log ,buffsym))
         (defun ,(intern (concat prefix "--log-open-log")) ()
           "Open buffer which name is \" *log4e-PREFIX*\"."
           (interactive)
           (log4e--open-log ,buffsym))
         (defun ,(intern (concat prefix "--log-open-log-if-debug")) ()
           "Open buffer which name is \" *log4e-PREFIX*\" if debugging is enabled."
           (log4e--open-log-if-debug ,buffsym ,dbgsym))
;;          (defun ,(intern (concat prefix "--log-send-report-if-not-debug")) ()
;;            "Send bug report to author if debugging is disabled.

;; The author mailaddress is set by `PREFIX--log-set-author-mail-address'.
;; About the way of sending bug report, see `reporter-submit-bug-report'."
;;            (log4e--send-report-if-not-debug ,buffsym ,dbgsym ,addrsym ,prefix))
         ))))

(define-derived-mode log4e-mode view-mode "Log4E"
  ""
  (define-key log4e-mode-map (kbd "J") 'log4e:next-log)
  (define-key log4e-mode-map (kbd "K") 'log4e:previous-log))

(defun log4e:next-log ()
  "Move to start of next log on log4e-mode."
  (interactive)
  (let* ((level))
    (while (and (not level)
                (< (point) (point-max)))
      (forward-line 1)
      (setq level (log4e--get-current-log-line-level)))
    level))

(defun log4e:previous-log ()
  "Move to start of previous log on log4e-mode."
  (interactive)
  (let* ((level))
    (while (and (not level)
                (> (point) (point-min)))
      (forward-line -1)
      (setq level (log4e--get-current-log-line-level)))
    level))

(defun log4e:insert-start-log-quickly ()
  "Insert logging statment for trace level log at start of current function/macro."
  (interactive)
  (let* ((fstartpt (when (re-search-backward "(\\(?:defun\\|defmacro\\)\\*? +\\([^ ]+\\) +(\\([^)]*\\))" nil t)
                     (point)))
         (fncnm (when fstartpt (match-string-no-properties 1)))
         (argtext (when fstartpt (match-string-no-properties 2)))
         (prefix (save-excursion
                   (goto-char (point-min))
                   (loop while (re-search-forward "(log4e:deflogger[ \n]+\"\\([^\"]+\\)\"" nil t)
                         for prefix = (match-string-no-properties 1)
                         for currface = (get-text-property (match-beginning 0) 'face)
                         if (not (eq currface 'font-lock-comment-face))
                         return prefix))))
    (when (and fstartpt prefix)
      (let* ((fncnm (replace-regexp-in-string (concat "\\`" prefix "[^a-zA-Z0-9]+") "" fncnm))
             (fncnm (replace-regexp-in-string "-" " " fncnm))
             (argtext (replace-regexp-in-string "\n" " " argtext))
             (argtext (replace-regexp-in-string "^ +" "" argtext))
             (argtext (replace-regexp-in-string " +$" "" argtext))
             (args (split-string argtext " +"))
             (args (loop for arg in args
                         if (and (not (string= arg ""))
                                 (not (string-match "\\`&" arg)))
                         collect arg))
             (logtext (loop with ret = (format "start %s." fncnm)
                            for arg in args
                            do (setq ret (concat ret " " arg "[%s]"))
                            finally return ret))
             (sexpformat (loop with ret = "(%s--log 'trace \"%s\""
                               for arg in args
                               do (setq ret (concat ret " %s"))
                               finally return (concat ret ")")))
             (inserttext (apply 'format sexpformat prefix logtext args)))
        (forward-char)
        (forward-sexp 3)
        (when (re-search-forward "\\=[ \n]+\"" nil t)
          (forward-char -1)
          (forward-sexp))
        (newline-and-indent)
        (insert inserttext)))))


(defun log4e--get-current-log-line-level ()
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) 'log4e--level)))

(defun log4e--logging (buffnm codesys msgtmpl timetmpl minlevel maxlevel tgl level msg &rest msgargs)
  (let* ((buff (log4e--get-or-create-log-buffer buffnm codesys)))
    (when (log4e--doing-p minlevel maxlevel level tgl)
      (save-match-data
        (with-current-buffer buff
          (let* ((timetext (format-time-string timetmpl))
                 (lvltext (format "%-05s" (upcase (symbol-name level)))))
            (put-text-property 0 (length timetext) 'face 'font-lock-doc-face timetext)
            (put-text-property 0 (length lvltext) 'face 'font-lock-keyword-face lvltext)
            (let* ((logtext msgtmpl)
                   (logtext (replace-regexp-in-string "%t" timetext logtext))
                   (logtext (replace-regexp-in-string "%l" lvltext logtext))
                   (logtext (replace-regexp-in-string "%m" msg logtext))
                   (startpt (progn (goto-char (point-max))
                                   (point))))
              (setq buffer-read-only nil)
              (insert logtext "\n")
              (put-text-property startpt (+ startpt 1) 'log4e--level level)
              (goto-char startpt)
              (while (re-search-forward "\\(%[a-zA-Z]\\)" nil t)
                (let* ((currtype (match-string-no-properties 1))
                       (currarg (pop msgargs))
                       (failfmt)
                       (currtext (condition-case e
                                     (format currtype currarg)
                                   (error (setq failfmt t)
                                          (format "=%s=" (error-message-string e))))))
                  (ignore-errors
                    (cond (failfmt (put-text-property 0 (length currtext) 'face 'font-lock-warning-face currtext))
                          (t       (put-text-property 0 (length currtext) 'face 'font-lock-string-face currtext))))
                  (replace-match currtext t t)))
              (goto-char (point-max))
              nil)))))))

(defun log4e--get-or-create-log-buffer (buffnm &optional codesys)
  (or (get-buffer buffnm)
      (let* ((buff (get-buffer-create buffnm)))
        (with-current-buffer buff
          (log4e-mode)
          (when codesys
            (setq buffer-file-coding-system codesys)))
        buff)))

(defun log4e--clear-log (buffnm)
  (with-current-buffer (log4e--get-or-create-log-buffer buffnm)
    (setq buffer-read-only nil)
    (erase-buffer)))

(defun log4e--open-log (buffnm)
  (let* ((buff (get-buffer buffnm)))
    (if (not (buffer-live-p buff))
        (message "[Log4E] Not exist log buffer.")
      (with-current-buffer buff
        (setq buffer-read-only t))
      (pop-to-buffer buff))))

(defun log4e--open-log-if-debug (buffnm dbg)
  (when dbg
    (log4e--open-log buffnm)))

;; (defun log4e--send-report-if-not-debug (buffnm dbg addr prefix)
;;   (let* ((buff (get-buffer buffnm)))
;;     (when (and (not dbg)
;;                (stringp addr)
;;                (buffer-live-p buff))
;;       (reporter-submit-bug-report addr prefix nil nil nil nil))))

(defun log4e--doing-p (minlevel maxlevel currlevel tgl)
  (let* ((minlvlvalue (or (assoc-default minlevel log4e-log-level-alist)
                          1))
         (maxlvlvalue (or (assoc-default maxlevel log4e-log-level-alist)
                          6))
         (currlvlvalue (or (assoc-default currlevel log4e-log-level-alist)
                           0)))
    (and tgl
         (>= currlvlvalue minlvlvalue)
         (<= currlvlvalue maxlvlvalue))))


(provide 'log4e)
;;; log4e.el ends here
