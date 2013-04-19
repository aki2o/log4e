(require 'log4e)
(require 'el-expectations)


(expectations
  (desc "logging default")
  (expect ""
    (makunbound 'log4e--toggle-logging-hoge)
    (log4e:deflogger "hoge" "%m" "%S")
    (hoge--log-set-level 'trace 'fatal)
    (hoge--log-clear-log)
    (hoge--log-fatal "test for fatal.")
    (hoge--log-error "test for error.")
    (hoge--log-warn  "test for warn.")
    (hoge--log-info  "test for info.")
    (hoge--log-debug "test for debug.")
    (hoge--log-trace "test for trace.")
    (with-current-buffer " *log4e-hoge*"
      (buffer-string))))

(expectations
  (desc "logging enable")
  (expect "test for fatal.\ntest for error.\ntest for warn.\ntest for info.\ntest for debug.\ntest for trace.\n"
    (makunbound 'log4e--toggle-logging-hoge)
    (makunbound 'log4e--log-templete-hoge)
    (makunbound 'log4e--time-templete-hoge)
    (log4e:deflogger "hoge" "%m" "%S")
    (hoge--log-set-level 'trace 'fatal)
    (hoge--log-enable-logging)
    (hoge--log-clear-log)
    (hoge--log-fatal "test for fatal.")
    (hoge--log-error "test for error.")
    (hoge--log-warn  "test for warn.")
    (hoge--log-info  "test for info.")
    (hoge--log-debug "test for debug.")
    (hoge--log-trace "test for trace.")
    (with-current-buffer " *log4e-hoge*"
      (buffer-string))))

(expectations
  (desc "logging disable")
  (expect ""
    (makunbound 'log4e--toggle-logging-hoge)
    (log4e:deflogger "hoge" "%m" "%S")
    (hoge--log-set-level 'trace 'fatal)
    (hoge--log-enable-logging)
    (hoge--log-disable-logging)
    (hoge--log-clear-log)
    (hoge--log-fatal "test for fatal.")
    (hoge--log-error "test for error.")
    (hoge--log-warn  "test for warn.")
    (hoge--log-info  "test for info.")
    (hoge--log-debug "test for debug.")
    (hoge--log-trace "test for trace.")
    (with-current-buffer " *log4e-hoge*"
      (buffer-string))))

