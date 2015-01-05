(require 'log4e)
(require 'el-expectations)


(expectations
  (desc "logformat 1")
  (expect 0
    (makunbound 'log4e--log-template-hoge)
    (makunbound 'log4e--time-template-hoge)
    (log4e:deflogger "hoge" "%t [%l] %m" "%H:%M:%S")
    (hoge--log-set-level 'fatal 'fatal)
    (hoge--log-enable-logging)
    (hoge--log-clear-log)
    (hoge--log-fatal "test for %s : %s." "hoge" "fatal")
    (with-current-buffer " *log4e-hoge*"
      (string-match "\\`[0-9][0-9]:[0-9][0-9]:[0-9][0-9] \\[FATAL\\] test for hoge : fatal\\.\n\\'"
                    (buffer-string))))
  (desc "logformat 2")
  (expect 0
    (makunbound 'log4e--log-template-hoge)
    (makunbound 'log4e--time-template-hoge)
    (log4e:deflogger "hoge" "%l:[%t] %m" "%y/%m/%d %H:%M:%S")
    (hoge--log-set-level 'info 'info)
    (hoge--log-enable-logging)
    (hoge--log-clear-log)
    (hoge--log-info "test for %s." "hoge")
    (with-current-buffer " *log4e-hoge*"
      (string-match "\\`INFO :\\[[0-9][0-9]/[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\] test for hoge\\.\n\\'"
                    (buffer-string))))
  (desc "logformat for each level")
  (expect "FATAL:It's fatal.\nERROR:It's error.\nWARN :It's warn.\nINFO :It's info.\nDEBUG:It's debug.\nTRACE:It's trace.\n"
    (makunbound 'log4e--log-template-hoge)
    (makunbound 'log4e--time-template-hoge)
    (log4e:deflogger "hoge" "%l:%m" "%H:%M:%S")
    (hoge--log-set-level 'trace 'fatal)
    (hoge--log-enable-logging)
    (hoge--log-clear-log)
    (hoge--log-fatal "It's fatal.")
    (hoge--log-error "It's error.")
    (hoge--log-warn  "It's warn.")
    (hoge--log-info  "It's info.")
    (hoge--log-debug "It's debug.")
    (hoge--log-trace "It's trace.")
    (with-current-buffer " *log4e-hoge*"
      (buffer-string)))
  )

