; vim: ft=lisp et
(in-package :asdf)
(defsystem "quaspar.test"
  :version
  "0.11.2"
  :depends-on
  (:jingoh "quaspar")
  :components
  ((:file "quaspar"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :quaspar args)))
