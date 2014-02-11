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
                   "test :param rest"
                   (list "lorem ipsum")))

(ert-deftest parse-body-string-in-the-beginning ()
  (with-parse-body "\"lorem ipsum\" test rest"
                   ":param test rest"
                   (list "lorem ipsum")))

(ert-deftest parse-body-string-in-the-end ()
  (with-parse-body "test rest \"lorem ipsum\""
                   "test rest :param"
                   (list "lorem ipsum")))

(ert-deftest parse-body-two-strings ()
  (with-parse-body "test \"best guest\" rest \"ipsum lorem\""
                   "test :param rest :param"
                   (list "best guest" "ipsum lorem")))

(ert-deftest parse-body-more-strings ()
  (with-parse-body
   "\"best guest\" rest \"ipsum lorem\" best \"single\" test \"Giuseppe Moscati\" guest \"Erich Fromm\""
   ":param rest :param best :param test :param guest :param"
   (list "best guest" "ipsum lorem" "single" "Giuseppe Moscati" "Erich Fromm")))

(ert-deftest parse-body-empty-string ()
  (with-parse-body "test \"\" best"
                   "test :param best"
                   (list "")))

(ert-deftest parse-body-just-empty-string ()
  (with-parse-body "\"\""
                   ":param"
                   (list "")))

(ert-deftest parse-body-two-strings-together ()
  (with-parse-body "test \"lorem ipsum\"\"best guest\" best"
                   "test :param:param best"
                   (list "lorem ipsum" "best guest")))

(ert-deftest parse-body-very-simple-case-with-escaped-double-quote ()
  (with-parse-body "test \"lorem \\\"ipsum\" rest"
                   "test :param rest"
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
                   "test :param :param rest"
                   (list "lorem \\\"ipsum" "ipsum \\\" \\\" lorem")))

(ert-deftest parse-body-with-more-escaped-slashes-and-double-quote ()
  (with-parse-body "test \"lorem \\\\\\\" ipsum\" rest"
                   "test :param rest"
                   (list "lorem \\\\\\\" ipsum")))

(ert-deftest parse-body-with-more-escaped-slashes-and-double-quote ()
  (with-parse-body "test \"lorem \\\\\\\" ipsum\" rest"
                   "test :param rest"
                   (list "lorem \\\\\\\" ipsum")))

(ert-deftest parse-body-with-symbol ()
  (with-parse-body "test 'best rest"
                   "test :param rest"
                   (list "best")))

(ert-deftest parse-body-with-symbol-at-the-beginning ()
  (with-parse-body "'best rest"
                   ":param rest"
                   (list "best")))

(ert-deftest parse-body-with-symbol-at-the-end ()
  (with-parse-body "best 'rest"
                   "best :param"
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
                   ":param :param"
                   (list "lorem ipsum" "rest")))

(ert-deftest parse-body-with-preceding-symbol-and-string ()
  (with-parse-body "'test \"lorem ipsum\""
                   ":param :param"
                   (list "test" "lorem ipsum")))

(ert-deftest parse-body-with-symbol-in-the-string ()
  (with-parse-body "\"lorem 'param ipsum\""
                   ":param"
                   (list "lorem 'param ipsum")))

(ert-deftest parse-body-with-empty-symbol ()
  (with-parse-body "test ' best"
                   "test :param best"
                   (list "")))

(ert-deftest parse-body-with-empty-quoted-symbol ()
  (with-parse-body "test \\' best"
                   "test \\' best"
                   (list)))

(ert-deftest parse-body-with-empty-quoted-slash-and-symbol ()
  (with-parse-body "test \\\\'best rest"
                   "test \\\\:param rest"
                   (list "best")))

(ert-deftest parse-body-with-many-quotes ()
  (with-parse-body "test ''''best rest"
                   "test :param rest"
                   (list "'''best")))
