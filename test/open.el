(require 'log4e)
(require 'el-expectations)


(expectations
  (desc "open log buffer")
  (expect " *log4e-hoge*"
    (log4e:deflogger "hoge" "%m" "%S")
    (hoge--log-enable-logging)
    (hoge--log-fatal "It's fatal.")
    (hoge--log-open-log)
    (buffer-name))
  (desc "open killed log buffer")
  (expect "[Log4E] Not exist log buffer."
    (kill-buffer " *log4e-hoge*")
    (hoge--log-open-log))
  (desc "open log buffer if debugging")
  (expect " *log4e-hoge*"
    (hoge--log-enable-logging)
    (hoge--log-enable-debugging)
    (hoge--log-fatal "It's fatal.")
    (hoge--log-open-log-if-debug)
    (buffer-name))
  (desc "open log buffer if debugging")
  (expect t
    (delete-window)
    (hoge--log-enable-logging)
    (hoge--log-enable-debugging)
    (hoge--log-disable-debugging)
    (hoge--log-fatal "It's fatal.")
    (hoge--log-open-log-if-debug)
    (not (string= (buffer-name) " *log4e-hoge*")))
  )

