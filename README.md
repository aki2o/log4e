What's this?
============

This is a extension of Emacs provides logging framework for Elisp.


Feature
=======

### Define function for logging automatically

Write the following sexp in your elisp file.

```lisp
(log4e:deflogger "hoge" "%t [%l] %m" "%H:%M:%S")
```

Then, you can use the following function/command.

* hoge--log-fatal
* hoge--log-error
* hoge--log-warn
* hoge--log-info
* hoge--log-debug
* hoge--log-trace
* hoge--log
* hoge--log-set-level
* hoge--log-enable-logging
* hoge--log-disable-logging
* hoge--log-enable-debugging
* hoge--log-disable-debugging
* hoge--log-debugging-p
* hoge--log-set-coding-system
* hoge--log-set-author-mail-address
* hoge--log-clear-log
* hoge--log-open-log
* hoge--log-open-log-if-debug

For detail, see Usage section.

### font-lock on logging buffer

The image of logging buffer is the following ...

![Demo1](image/demo1.png)

The following face is used for font-lock on logging buffer.

* font-lock-doc-face
* font-lock-keyword-face
* font-lock-string-face
* font-lock-warning-face

### key binding on logging buffer

The mode of logging buffer is log4e-mode which is based view-mode.  
The following binding is added.

* J - log4e:next-log ... move to head of next log
* K - log4e:previous-log ... move to head of previous log.


Install
=======

### If use package.el

2013/07/19 It's available by using melpa.  

### If use el-get.el

2013/07/26 It's available. But, master branch only.  

### If use auto-install.el

```lisp
(auto-install-from-url "https://raw.github.com/aki2o/log4e/master/log4e.el")
```

### Manually

Download log4e.el and put on your load-path.


Usage
=====

For example, develop elisp with prefix "hoge".

### Initially

Write the following in the elisp.

```lisp
(require 'log4e)
(log4e:deflogger "hoge" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                  (error . "error")
                                                  (warn  . "warn")
                                                  (info  . "info")
                                                  (debug . "debug")
                                                  (trace . "trace")))
```

`log4e:deflogger` receive the following arguments.

1. The elisp prefix.
2. Format of log. The following words has a special meaning in it.
    * %t ... Replaced with the 3rd argument.
    * %l ... Replaced with LogLevel. Its list is "TRACE", "DEBUG", "INFO", "WARN", "ERROR" and "FATAL".
    * %m ... Replaced with given message by the logging function. About them, see Coding section below.
3. Format of time. This value is passed to `format-time-string` and replaced with its returend.
4. Alist of the logging function name. This is optional. If nil, This value is `log4e-default-logging-function-name-alist`.

### Coding

Develop the elisp with logging.

```lisp
(defun hoge-do-hoge (hoge)
  (if (not (stringp hoge))
      (progn (hoge--fatal "failed do hoge : hoge is '%s'" hoge)
             (hoge--log-open-log-if-debug))
    (hoge--debug "start do hoge about '%s'" hoge)
    (message "hoge!")
    (hoge--info "done hoge about '%s'" hoge)))
```

The logging functions are named by the 4th argument of `log4e:deflogger`.  
The arguments of them are passed to `format` and its returned is used for the message part of log.  
The returned of them is always nil.

### Enable/Disable logging

By default, logging is disabled.  
For doing logging, use `hoge--log-enable-logging`.  
If you want to do logging anytime, write `hoge--log-enable-logging` in the elisp.  
For stopping logging, use `hoge--log-disable-logging`.

### Set range of logging level

By default, The logging range is from 'info' to 'fatal'.  
So, eval the following ...

```lisp
(hoge-do-hoge "HOGEGE")
(hoge--log-open-log)
```

Then, the buffer is displayed that named ' \*log4e-hoge\*'. And the string is like the following.

    12:34:56 [INFO ] done hoge about 'HOGEGE'

If you change the logging range, eval the following sexp.

```lisp
(hoge--log-set-level 'debug 'fatal)
```

Then, eval the following ...

```lisp
(hoge-do-hoge "FUGAGA")
(hoge--log-open-log)
```

Then, the buffer is displayed that named ' \*log4e-hoge\*'. And the string is like the following.

    12:34:56 [INFO ] done hoge about 'HOGEGE'
    12:35:43 [DEBUG] start do hoge about 'FUGAGA'
    12:35:43 [INFO ] done hoge about 'FUGAGA'

If you change the logging range anytime, write `hoge--log-set-level` in the elisp.  
`hoge--log-set-level` receive the following arguments.

1. The lowest level for doing logging. Its list is 'trace', 'debug', 'info', 'warn', 'error' and 'fatal'.
2. The highest level for doing logging. This is optional. If nil, This value is 'fatal'.

### For debug

When you debug the elisp, eval the following ...

```lisp
(hoge--log-enable-debugging)
(hoge-do-hoge 'hogege)
```

Then, the buffer is displayed that named ' \*log4e-hoge\*'. And the string is like the following.

    12:34:56 [INFO ] done hoge about 'HOGEGE'
    12:35:43 [DEBUG] start do hoge about 'FUGAGA'
    12:35:43 [INFO ] done hoge about 'FUGAGA'
    12:54:32 [FATAL] failed do hoge : hoge is 'hogege'

If you want to stop debugging, use `hoge--log-disable-debugging`.  
If you want to verify activity of debugging in the elisp, use `hoge--log-debugging-p`.

By using `hoge--log-enable-debugging`, logging is enabled too.

### Insert logging statement quickly

You can insert logging statement at your elisp by using `log4e:insert-start-log-quickly`.  
The command insert `(hoge--log ...)` to start of pointed function/macro at the time.  
If you want to bind some key to the command, write like the following in your .emacs or site-start.el file.

```lisp
(define-key emacs-lisp-mode-map (kbd "C-\\") 'log4e:insert-start-log-quickly)
```

### Other

If you want to clear the log buffer named ' \*log4e-hoge\*', use `hoge--log-clear-log`.  
If you want to do logging with changing log level by some condition locally, use `hoge--log`.

`hoge--log` is base of the logging function. About them, see Coding section above.  
It receive a log level as 1st argument.


Tested On
=========

* Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK


**Enjoy!!!**

