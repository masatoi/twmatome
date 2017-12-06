#|
  This file is a part of twmatome project.
|#

(defsystem "twmatome-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("twmatome"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "twmatome"))))
  :description "Test system for twmatome"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
