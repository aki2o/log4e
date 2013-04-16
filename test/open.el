(require 'log4e)
(require 'el-expectations)


(expectations
  (desc "open log buffer")
  (expect " *log4e-hoge*"
    (log4e:deflogger "hoge" "%m" "%S")
    (hoge--log-enable-logging)
    (hoge--log-fatal "It's fatal.")
    (let* ((ret ""))
      (ad-with-auto-activation-disabled
       (flet ((message (format-string &rest args)
                       (setq ret format-string))
              (pop-to-buffer (buf &optional other-window norecord)
                             (setq ret (buffer-name buf))))
         (hoge--log-open-log)
         ret)))))

(expectations
  (desc "open killed log buffer")
  (expect "[Log4E] Not exist log buffer."
    (log4e:deflogger "hoge" "%m" "%S")
    (kill-buffer " *log4e-hoge*")
    (let* ((ret ""))
      (ad-with-auto-activation-disabled
       (flet ((message (format-string &rest args)
                       (setq ret format-string))
              (pop-to-buffer (buf &optional other-window norecord)
                             (setq ret (buffer-name buf))))
         (hoge--log-open-log)
         ret)))))

(expectations
  (desc "open log buffer if debugging")
  (expect " *log4e-hoge*"
    (log4e:deflogger "hoge" "%m" "%S")
    (hoge--log-enable-logging)
    (hoge--log-enable-debugging)
    (hoge--log-fatal "It's fatal.")
    (let* ((ret ""))
      (ad-with-auto-activation-disabled
       (flet ((message (format-string &rest args)
                       (setq ret format-string))
              (pop-to-buffer (buf &optional other-window norecord)
                             (setq ret (buffer-name buf))))
         (hoge--log-open-log-if-debug)
         ret)))))

(expectations
  (desc "open log buffer if debugging")
  (expect ""
    (log4e:deflogger "hoge" "%m" "%S")
    (hoge--log-enable-logging)
    (hoge--log-enable-debugging)
    (hoge--log-disable-debugging)
    (hoge--log-fatal "It's fatal.")
    (let* ((ret ""))
      (ad-with-auto-activation-disabled
       (flet ((message (format-string &rest args)
                       (setq ret format-string))
              (pop-to-buffer (buf &optional other-window norecord)
                             (setq ret (buffer-name buf))))
         (hoge--log-open-log-if-debug)
         ret)))))

