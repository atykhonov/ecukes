(require 'ecukes-steps)

(defun with-ecukes-steps-format-step (body args expected-body)
  (should
   (equal (ecukes-steps-format-step body args)
          expected-body)))

(defun with-ecukes-steps-body-arguments (body args)
  (should
   (equal (ecukes-steps-body-arguments body)
          args)))

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
   (Given "a `arg-1' state" 'identity)
   (should (equal (Given "a 'known state") "known"))))

(ert-deftest steps-call-step-multiple-arguments ()
  "Should call step with multiple arguments."
  (with-steps
   (Given "state `arg-1' and `arg-2'"
          (lambda (state-1 state-2)
            (format "%s-%s" state-1 state-2)))
   (should (equal (Given "state `arg-1' and `arg-2'" "known" "unknown") "known-unknown"))))

(ert-deftest steps-call-step-line-arguments ()
  "Should call step with inline arguments."
  (with-steps
   (Given "state `arg-1' and `arg-2'"
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
    (mock (error (ansi-red "Step not defined: `a `arg-1' state`")) :times 1)
    (Given "a `arg-1' state" "known"))))

(ert-deftest steps-undefined-multiple-arguments ()
  "Should error when not defined, multiple arguments."
  (with-steps
   (with-mock
    (mock (error (ansi-red "Step not defined: `state `arg-1' and `arg-2'`")) :times 1)
    (Given "state `arg-1' and `arg-2'" "known" "unknown"))))

(ert-deftest steps-args-no-args ()
  "Should return empty list when no args."
  (with-steps
   (Given "a known state" 'ignore)
   (let ((step (mock-step "Given a known state")))
     (should (equal (ecukes-steps-args step) nil)))))

(ert-deftest steps-args-single-arg ()
  "Should return args when single arg."
  (with-steps
   (Given "state `arg-1'$" 'ignore)
   (let ((step (mock-step "Given state \"known\"")))
     (should (equal (ecukes-steps-args step) (list "known"))))))

(ert-deftest steps-args-multiple-args ()
  "Should return args when multiple args."
  (with-steps
   (Given "state `arg-1' and `arg-2'" 'ignore)
   (let ((step (mock-step "Given state \"known\" and \"unknown\"")))
     (should (equal (ecukes-steps-args step) (list "known" "unknown"))))))

(ert-deftest steps-args-without-quotes ()
  "Should return args when multiple (unquoted) args."
  (with-steps
   (Given "state `arg-1' and `arg-2'" 'ignore)
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
   "test `arg-1' rest"
   (list "my-param")
   "test \"my-param\" rest"))

(ert-deftest steps-format-step-several-arguments ()
  (with-ecukes-steps-format-step
   "test `arg-1' best `arg-2' rest `arg-3' guest"
   (list "my-param" "next-param" "guest-param")
   "test \"my-param\" best \"next-param\" rest \"guest-param\" guest"))

(ert-deftest steps-format-step-argument-at-the-beginning ()
  (with-ecukes-steps-format-step
   "`arg-1' best"
   (list "my-param")
   "\"my-param\" best"))

(ert-deftest steps-format-step-argument-at-the-end ()
  (with-ecukes-steps-format-step
   "test `arg-1'"
   (list "my-param")
   "test \"my-param\""))

(ert-deftest steps-body-arguments ()
  (with-ecukes-steps-body-arguments "some `test' text `input' to the `buffer'"
                                    '("test" "input" "buffer")))

(ert-deftest define-step-simple ()
  (let ((expected nil)
        (definition nil))
    (define-step "some `test' case"
      (setq expected test))
    (When "some `test' case" "value")
    (should (equal expected "value"))))

(ert-deftest define-step-two-arguments ()
  (let ((expected-1 nil)
        (expected-2 nil)
        (definition nil))
    (define-step "some `test' case `arg'"
      (setq expected-1 test)
      (setq expected-2 arg))
    (When "some `test' case `arg'" "foo" "bar")
    (should (equal expected-1 "foo"))
    (should (equal expected-2 "bar"))))

(ert-deftest define-step-no-arguments ()
  (let ((expected nil)
        (definition nil))
    (define-step "some case"
      (setq expected "value"))
    (When "some case")
    (should (equal expected "value"))))

(ert-deftest ecukes-steps-format-body ()
   (equal (ecukes-steps-format-body "I go to point `point'")
          "I go to point `arg-1'"))

(ert-deftest ecukes-define-step-solide-text ()
  (let ((test))
    (define-step "solide text"
      (setq test t))
    (When "solide text")
    (should (equal test t))))

(ert-deftest ecukes-define-step-text-with-single-param ()
  (let ((test))
    (define-step "simple `bar' case"
      (setq test bar))
    (When "simple \"hello world\" case")
    (should (equal test "hello world"))))

(ert-deftest ecukes-define-step-text-with-several-params ()
  (let ((test1) (test2))
    (define-step "simple `foo' case `bar'"
      (setq test1 foo)
      (setq test2 bar))
    (When "simple \"hello world\" case \"lorem ipsum\"")
    (should (equal test1 "hello world"))
    (should (equal test2 "lorem ipsum"))))

(ert-deftest ecukes-define-step-text-quoted-param ()
  (let ((test))
    (define-step "simple `foo' case"
      (setq test foo))
    (When "simple 'hello case")
    (should (equal test "hello"))))

;; (ert-deftest ecukes-define-step-text-slashed-param ()
;;   (let ((test))
;;     (setq ecukes-steps-definitions nil)
;;     (define-step "simple \`foo' case"
;;       (setq test t))
;;     (When "simple `foo' case")
;;     (should (equal test t))))

;; (ert-deftest ecukes-define-step-any-text ()
;;   (let ((test))
;;     (setq ecukes-steps-definitions nil)
;;     (define-step "simple `foo the the' case"
;;       (setq test t))
;;     (When "simple 'foo case")
;;     (should (equal test t))))

(ert-deftest ecukes-define-step-call-with-parameter ()
  (let ((test))
    (setq ecukes-steps-definitions nil)
    (define-step "simple `foo' case"
      (setq test foo))
    (When "simple `foo' case" "hello world")
    (should (equal test "hello world"))))

;; (ert-deftest ecukes-define-step-mixed-call ()
;;   (let ((test1) (test2))
;;     (setq ecukes-steps-definitions nil)
;;     (define-step "simple `foo' case `bar' other"
;;       (setq test1 foo)
;;       (setq test2 bar))
;;     (When "simple `foo' case 'lorem" "hello world")
;;     (should (equal test1 "hello world"))
;;     (should (equal test2 "lorem"))))

(ert-deftest ecukes-define-step-pystring ()
  (let ((test))
    (setq ecukes-steps-definitions nil)
    (define-step "simple case:"
      (setq test pystring))
    (When "simple case:" "hello world")
    (should (equal test "hello world"))))

(ert-deftest ecukes-define-step-pystring-and-table ()
  (let ((test1) (test2))
    (setq ecukes-steps-definitions nil)
    (define-step "simple case:"
      (setq test1 pystring)
      (setq test2 table))
    (When "simple case:" "hello world" "lorem ipsum")
    (should (equal test1 "hello world"))
    (should (equal test2 "lorem ipsum"))))

(ert-deftest ecukes-define-step-pystring-and-param ()
  (let ((test1) (test2))
    (setq ecukes-steps-definitions nil)
    (define-step "simple `foo' case:"
      (setq test1 foo)
      (setq test2 pystring))
    (When "simple `foo' case:" "hello world" "lorem ipsum")
    (should (equal test1 "hello world"))
    (should (equal test2 "lorem ipsum"))))

(ert-deftest ecukes-define-step-with-t-expression ()
  (let ((test))
    (setq ecukes-steps-definitions nil)
    (define-step "simple `var' case"
      (setq test var))
    (When "simple 't case")
    (should (equal test t))))

(ert-deftest ecukes-define-step-with-list-expression-1 ()
  (let ((test))
    (setq ecukes-steps-definitions nil)
    (define-step "simple `var' case"
      (setq test var))
    (When "simple '(\"foo\" \"bar\") case")
    (should (equal test '("foo" "bar")))))

(ert-deftest ecukes-define-step-with-list-expression-2 ()
  (let ((test))
    (setq ecukes-steps-definitions nil)
    (define-step "simple `var' case"
      (setq test var))
    (When "simple '(list \"foo\" \"bar\") case")
    (should (equal test '(list "foo" "bar")))))

(ert-deftest ecukes-define-step-with-list-expression-3 ()
  (let ((test))
    (setq ecukes-steps-definitions nil)
    (define-step "simple `var' case"
      (setq test var))
    (When "simple '(\"foo\" . \"bar\") case")
    (should (equal test '("foo" . "bar")))))

;; (ert-deftest ecukes-define-step-with-list-expression-4 ()
;;   (let ((test))
;;     (setq ecukes-steps-definitions nil)
;;     (define-step "simple `var' case"
;;       (setq test var))
;;     (When "simple '(3) case")
;;     (should (equal test 3))))

(ert-deftest ecukes-define-step-with-list-expression-5 ()
  (let ((test))
    (setq ecukes-steps-definitions nil)
    (define-step "simple `var' case"
      (setq test var))
    (When "simple 'nil case")
    (should (equal test nil))))

(ert-deftest ecukes-define-step-pass-integer ()
  (let ((test))
    (setq ecukes-steps-definitions nil)
    (define-step "simple `var' case"
      (setq test var))
    (When "simple `var' case" 3)
    (should (equal test 3))))

(ert-deftest ecukes-define-step-pass-nil ()
  (let ((test))
    (setq ecukes-steps-definitions nil)
    (define-step "simple `var' case"
      (setq test var))
    (When "simple `var' case" nil)
    (should (equal test nil))))

(ert-deftest ecukes-define-step-pass-list ()
  (let ((test))
    (setq ecukes-steps-definitions nil)
    (define-step "simple `var' case"
      (setq test var))
    (When "simple `var' case" (list "test" "best"))
    (should (equal test (list "test" "best")))))

(ert-deftest ecukes-define-step-pass-t ()
  (let ((test))
    (setq ecukes-steps-definitions nil)
    (define-step "simple `var' case"
      (setq test var))
    (When "simple `var' case" t)
    (should (equal test t))))
