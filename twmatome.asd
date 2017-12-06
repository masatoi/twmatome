#|
  This file is a part of twmatome project.
|#

(defsystem "twmatome"
  :version "0.1.0"
  :author "Satoshi Imai"
  :license "MIT"
  :depends-on (:chirp :cl-markup :cl-heap :anaphora
               ;; :woo :clack :ningle
                      )
  :components ((:module "src"
                :components
                ((:file "twmatome")
                 ;; (:file "webpage")
                 )))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.org"))
  :in-order-to ((test-op (test-op "twmatome-test"))))
