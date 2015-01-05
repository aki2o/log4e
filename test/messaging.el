(require 'log4e)
(require 'el-expectations)

(expectations
  (desc "messaging default")
  (expect ""
    (makunbound 'log4e--messaging-buffer-hoge)
    (log4e:deflogger "hoge" "%m" "%S")
    (hoge--log-set-level 'trace 'fatal)
    (hoge--log-enable-logging)
    (let ((pt (with-current-buffer (get-buffer "*Messages*")
                (point))))
      (hoge--log-fatal "test for fatal.")
      (hoge--log-error "test for error.")
      (hoge--log-warn  "test for warn.")
      (hoge--log-info  "test for info.")
      (hoge--log-debug "test for debug.")
      (hoge--log-trace "test for trace.")
      (with-current-buffer (get-buffer "*Messages*")
        (buffer-substring-no-properties pt (point)))))
  (desc "messaging enable default")
  (expect "test for fatal.
test for error.
test for warn.
test for info.
test for debug.
test for trace.
"
    (hoge--log-enable-messaging)
    (let ((pt (with-current-buffer (get-buffer "*Messages*")
                (point)))
          (standard-output (get-buffer "*Messages*")))
      (hoge--log-fatal "test for fatal.")
      (hoge--log-error "test for error.")
      (hoge--log-warn  "test for warn.")
      (hoge--log-info  "test for info.")
      (hoge--log-debug "test for debug.")
      (hoge--log-trace "test for trace.")
      (with-current-buffer (get-buffer "*Messages*")
        (buffer-substring-no-properties pt (point)))))
  (desc "messaging enable custom")
  (expect "test for fatal.\n"
    (let ((buf (generate-new-buffer " *Log4E*")))
      (hoge--log-enable-messaging buf)
      (hoge--log-fatal "test for fatal.")
      (with-current-buffer buf
        (buffer-string))))
  (desc "messaging disable")
  (expect ""
    (hoge--log-disable-messaging)
    (let ((pt (with-current-buffer (get-buffer "*Messages*")
                (point))))
      (hoge--log-fatal "test for fatal.")
      (with-current-buffer (get-buffer "*Messages*")
        (buffer-substring-no-properties pt (point)))))
  )
