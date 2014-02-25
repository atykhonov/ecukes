(defun with-parse-body (body expected-body parameters)
  (let ((parsed-body (ecukes-parse-body body)))
    (should (equal (car parsed-body)
                   expected-body))
    (should (equal (cadr parsed-body)
                   parameters))))

(ert-deftest parse-body-clear-text ()
  (with-parse-body "test" "test" (list)))

(ert-deftest parse-body-string-in-the-middle ()
  (with-parse-body "test \"lorem ipsum\" rest"
                   "test `arg-1' rest"
                   (list "lorem ipsum")))

(ert-deftest parse-body-string-in-the-beginning ()
  (with-parse-body "\"lorem ipsum\" test rest"
                   "`arg-1' test rest"
                   (list "lorem ipsum")))

(ert-deftest parse-body-string-in-the-end ()
  (with-parse-body "test rest \"lorem ipsum\""
                   "test rest `arg-1'"
                   (list "lorem ipsum")))

(ert-deftest parse-body-integer-in-the-end ()
  (with-parse-body "test rest \"11\""
                   "test rest `arg-1'"
                   (list "11")))

(ert-deftest parse-body-two-strings ()
  (with-parse-body "test \"best guest\" rest \"ipsum lorem\""
                   "test `arg-1' rest `arg-2'"
                   (list "best guest" "ipsum lorem")))

(ert-deftest parse-body-more-strings ()
  (with-parse-body
   "\"best guest\" rest \"ipsum lorem\" best \"single\" test \"Giuseppe Moscati\" guest \"Erich Fromm\""
   "`arg-1' rest `arg-2' best `arg-3' test `arg-4' guest `arg-5'"
   (list "best guest" "ipsum lorem" "single" "Giuseppe Moscati" "Erich Fromm")))

(ert-deftest parse-body-empty-string ()
  (with-parse-body "test \"\" best"
                   "test `arg-1' best"
                   (list "")))

(ert-deftest parse-body-two-strings-and-empty-string ()
  (with-parse-body "test \"\" best \"guest\" rest"
                   "test `arg-1' best `arg-2' rest"
                   (list "" "guest")))

(ert-deftest parse-body-just-empty-string ()
  (with-parse-body "\"\""
                   "`arg-1'"
                   (list "")))

(ert-deftest parse-body-two-strings-together ()
  (with-parse-body "test \"lorem ipsum\"\"best guest\" best"
                   "test `arg-1'`arg-2' best"
                   (list "lorem ipsum" "best guest")))

(ert-deftest parse-body-very-simple-case-with-escaped-double-quote ()
  (with-parse-body "test \"lorem \\\"ipsum\" rest"
                   "test `arg-1' rest"
                   (list "lorem \\\"ipsum")))

(ert-deftest parse-body-escape-quote-at-the-beginning ()
  (with-parse-body "\\\"test best"
                   "\\\"test best"
                   (list)))

(ert-deftest parse-body-escape-quote-at-the-end ()
  (with-parse-body "test best\\\""
                   "test best\\\""
                   (list)))

(ert-deftest parse-body-with-more-escaped-double-quotes ()
  (with-parse-body "test \"lorem \\\"ipsum\" \"ipsum \\\" \\\" lorem\" rest"
                   "test `arg-1' `arg-2' rest"
                   (list "lorem \\\"ipsum" "ipsum \\\" \\\" lorem")))

(ert-deftest parse-body-with-more-escaped-slashes-and-double-quote ()
  (with-parse-body "test \"lorem \\\\\\\" ipsum\" rest"
                   "test `arg-1' rest"
                   (list "lorem \\\\\\\" ipsum")))

(ert-deftest parse-body-with-more-escaped-slashes-and-double-quote ()
  (with-parse-body "test \"lorem \\\\\\\" ipsum\" rest"
                   "test `arg-1' rest"
                   (list "lorem \\\\\\\" ipsum")))

(ert-deftest parse-body-with-symbol ()
  (with-parse-body "test 'best rest"
                   "test `arg-1' rest"
                   (list "best")))

(ert-deftest parse-body-nil-symbol ()
  (with-parse-body "test 'nil rest"
                   "test `arg-1' rest"
                   (list nil)))

(ert-deftest parse-body-t-symbol ()
  (with-parse-body "test 't rest"
                   "test `arg-1' rest"
                   (list t)))

(ert-deftest parse-body-with-symbol-at-the-beginning ()
  (with-parse-body "'best rest"
                   "`arg-1' rest"
                   (list "best")))

(ert-deftest parse-body-with-symbol-at-the-end ()
  (with-parse-body "best 'rest"
                   "best `arg-1'"
                   (list "rest")))

(ert-deftest parse-body-with-quoted-symbol-in-the-middle ()
  (with-parse-body "test \\'rest best"
                   "test \\'rest best"
                   (list)))

(ert-deftest parse-body-with-quoted-symbol-at-the-and ()
  (with-parse-body "best \\'rest"
                   "best \\'rest"
                   (list)))

(ert-deftest parse-body-with-quoted-symbol-at-the-beginning ()
  (with-parse-body "\\'rest best"
                   "\\'rest best"
                   (list)))

(ert-deftest parse-body-with-preceding-string-and-symbol ()
  (with-parse-body "\"lorem ipsum\" 'rest"
                   "`arg-1' `arg-2'"
                   (list "lorem ipsum" "rest")))

(ert-deftest parse-body-with-preceding-symbol-and-string ()
  (with-parse-body "'test \"lorem ipsum\""
                   "`arg-1' `arg-2'"
                   (list "test" "lorem ipsum")))

(ert-deftest parse-body-with-symbol-in-the-string ()
  (with-parse-body "\"lorem 'param ipsum\""
                   "`arg-1'"
                   (list "lorem 'param ipsum")))

(ert-deftest parse-body-with-empty-symbol ()
  (with-parse-body "test ' best"
                   "test `arg-1' best"
                   (list "")))

(ert-deftest parse-body-with-empty-quoted-symbol ()
  (with-parse-body "test \\' best"
                   "test \\' best"
                   (list)))

(ert-deftest parse-body-with-empty-quoted-slash-and-symbol ()
  (with-parse-body "test \\\\'best rest"
                   "test \\\\`arg-1' rest"
                   (list "best")))

(ert-deftest parse-body-with-many-quotes ()
  (with-parse-body "test ''''best rest"
                   "test `arg-1' rest"
                   (list "'''best")))

(ert-deftest parse-body-with-the-args-same-values ()
  (with-parse-body "test 'best rest 'best guest 'best"
                   "test `arg-1' rest `arg-2' guest `arg-3'"
                   (list "best" "best" "best")))

(ert-deftest parse-body-with-expression-list ()
  (with-parse-body "test '(\"test\" \"best\" \"rest\") rest"
                   "test `arg-1' rest"
                   (list (list "test" "best" "rest"))))
