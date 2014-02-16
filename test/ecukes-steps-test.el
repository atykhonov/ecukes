(require 'ecukes-steps)

(defun with-ecukes-steps-format-step (body args expected-body)
  (should
   (equal (ecukes-steps-format-step body args)
          expected-body)))

(ert-deftest steps-define-step ()
  "Should define step."
  (with-steps
   (let ((load-file-name "DUMMY/FILE/NAME.el"))
     (Given "a known state" 'ignore))
   (should
    (equal
     (make-ecukes-step-def :body "a known state" :fn 'ignore
                           :file "DUMMY/FILE/NAME.el")
     (car ecukes-steps-definitions)))))

(ert-deftest steps-defined-with-doc ()
  "Should record docstring if given."
  (with-steps
   (Given "a known state"
     "This step does what."
     'ignore)
   (should
    (equal (ecukes-step-def-doc (car ecukes-steps-definitions))
           "This step does what."))))

(ert-deftest steps-define-same-step-twice ()
  "Should not define same step twice."
  (with-steps
   (Given "a known state" 'ignore)
   (Given "a known state" 'ignore)
   (should
    (equal (length ecukes-steps-definitions) 1))))

(ert-deftest steps-call-step-no-arguments ()
  "Should call step with no arguments."
  (with-steps
   (Given "a known state" (lambda () "x"))
   (should (equal (Given "a known state") "x"))))

(ert-deftest steps-call-step-single-argument ()
  "Should call step with single argument."
  (with-steps
   (Given "a :arg-1 state" 'identity)
   (should (equal (Given "a 'known state") "known"))))

(ert-deftest steps-call-step-multiple-arguments ()
  "Should call step with multiple arguments."
  (with-steps
   (Given "state :arg-1 and :arg-2"
          (lambda (state-1 state-2)
            (format "%s-%s" state-1 state-2)))
   (should (equal (Given "state :arg-1 and :arg-2" "known" "unknown") "known-unknown"))))

(ert-deftest steps-call-step-line-arguments ()
  "Should call step with inline arguments."
  (with-steps
   (Given "state :arg-1 and :arg-2"
          (lambda (state-1 state-2)
            (format "%s-%s" state-1 state-2)))
   (should (equal (Given "state 'known and 'unknown") "known-unknown"))))

(ert-deftest steps-undefined-no-arguments ()
  "Should error when not defined, no arguments."
  (with-steps
   (with-mock
    (mock (error (ansi-red "Step not defined: `a known state`")) :times 1)
    (Given "a known state"))))

(ert-deftest steps-undefined-single-argument ()
  "Should error when not defined, single argument."
  (with-steps
   (with-mock
    (mock (error (ansi-red "Step not defined: `a known state`")) :times 1)
    (Given "a %s state" "known"))))

(ert-deftest steps-undefined-multiple-arguments ()
  "Should error when not defined, multiple arguments."
  (with-steps
   (with-mock
    (mock (error (ansi-red "Step not defined: `state known and unknown`")) :times 1)
    (Given "state %s and %s" "known" "unknown"))))

(ert-deftest steps-args-no-args ()
  "Should return empty list when no args."
  (with-steps
   (Given "^a known state$" 'ignore)
   (let ((step (mock-step "Given a known state")))
     (should (equal (ecukes-steps-args step) nil)))))

(ert-deftest steps-args-single-arg ()
  "Should return args when single arg."
  (with-steps
   (Given "^state \"\\(.+\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\"")))
     (should (equal (ecukes-steps-args step) (list "known"))))))

(ert-deftest steps-args-multiple-args ()
  "Should return args when multiple args."
  (with-steps
   (Given "^state \"\\(.+\\)\" and \"\\(.+\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\" and \"unknown\"")))
     (should (equal (ecukes-steps-args step) (list "known" "unknown"))))))

(ert-deftest steps-args-without-quotes ()
  "Should return args when multiple (unquoted) args."
  (with-steps
   (Given "state :arg-1 and :arg-2" 'ignore)
   (let ((step (mock-step "Given state 'known and 'unknown")))
     (should (equal (ecukes-steps-args step) (list "known" "unknown"))))))

(ert-deftest steps-args-not-defined-no-arg ()
  "Should return quoted args when not defined when no args."
  (with-steps
   (let ((step (mock-step "Given state known")))
     (should (equal (ecukes-steps-args step) nil)))))

(ert-deftest steps-args-not-defined-args ()
  "Should return quoted args when not defined when args."
  (with-steps
   (let ((step (mock-step "Given state \"known\" and \"unknown\"")))
     (should (equal (ecukes-steps-args step) (list "known" "unknown"))))))

(ert-deftest steps-format-step-single-argument ()
  (with-ecukes-steps-format-step
   "test :param rest"
   (list "my-param")
   "test \"my-param\" rest"))

(ert-deftest steps-format-step-several-arguments ()
  (with-ecukes-steps-format-step
   "test :param best :param rest :param guest"
   (list "my-param" "next-param" "guest-param")
   "test \"my-param\" best \"next-param\" rest \"guest-param\" guest"))

(ert-deftest steps-format-step-argument-at-the-beginning ()
  (with-ecukes-steps-format-step
   ":param best"
   (list "my-param")
   "\"my-param\" best"))

(ert-deftest steps-format-step-argument-at-the-end ()
  (with-ecukes-steps-format-step
   "test :param"
   (list "my-param")
   "test \"my-param\""))
