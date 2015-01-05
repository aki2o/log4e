(require 'log4e)
(require 'el-expectations)


(expectations
  (desc "debugging default")
  (expect nil
    (makunbound 'log4e--toggle-debugging-hoge)
    (log4e:deflogger "hoge" "%t [%l] %m" "%H:%M:%S")
    (hoge--log-debugging-p))
  (desc "debugging enable")
  (expect t
    (makunbound 'log4e--toggle-debugging-hoge)
    (log4e:deflogger "hoge" "%t [%l] %m" "%H:%M:%S")
    (hoge--log-enable-debugging)
    (hoge--log-debugging-p))
  (desc "debugging disable")
  (expect nil
    (makunbound 'log4e--toggle-debugging-hoge)
    (log4e:deflogger "hoge" "%t [%l] %m" "%H:%M:%S")
    (hoge--log-enable-debugging)
    (hoge--log-disable-debugging)
    (hoge--log-debugging-p))
  )

