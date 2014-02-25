;;; ecukes-steps.el --- Functions to define and call step definitions

(eval-when-compile (require 'cl))

(require 'ansi)

(require 'ecukes-parse)

(defvar ecukes-steps-definitions nil
  "All defined step definitions.")

;;;###autoload
(defmacro define-step (step &optional docstring &rest body)
  `(define--step ,step nil ,docstring ,@body))

;;;###autoload
(defmacro define-step-async (step &optional docstring &rest body)
  `(define--step ,step t ,docstring ,@body))

(defmacro define--step (step async &optional docstring &rest body)
  (declare (doc-string 2) (indent defun))

  (if docstring (setq body (cons docstring body))
    (if (null body) (setq body '(nil))))

  (let ((arguments (ecukes-steps-body-arguments step))
        (params (list)))
    (when async
      (add-to-list 'arguments "callback" t))
    (dolist (arg arguments)
      (when (equal arg "pystring")
        (setq params
              (add-to-list 'params '&optional t)))
      (setq params
            (nconc params `(,(intern arg)))))
    `(ecukes-steps-define
      ,step
      (lambda (,@params) ,@body)
      ,nil nil ,async)))

;;;###autoload
(defalias 'Given 'ecukes-steps-define-or-call-step
  "Put the system in a known state.")

;;;###autoload
(defalias 'When 'ecukes-steps-define-or-call-step
  "Describe the key action.")

;;;###autoload
(defalias 'Then 'ecukes-steps-define-or-call-step
  "Observe outcomes.")

;;;###autoload
(defalias 'And 'ecukes-steps-define-or-call-step
  "Make Given/When/Then read more fluently.")

;;;###autoload
(defalias 'But 'ecukes-steps-define-or-call-step
  "Make Given/When/Then read more fluently.")

;;;###autoload
(defun ecukes-steps-define-or-call-step (body &rest args)
  "Define or call step.

When *defining* a step, argument takes the following form:
    (STEP-REGEXP [DOC] FUNCTION)
where STEP-REGEXP is a regular expression defining a step and
FUNCTION is the definition of the step.  You can optionally
give a docstring DOC as the second argument.

When *calling* a step, argument takes the following form:
    (STEP-BODY [ARG [ARG ..]])

\(fn STEP-REGEXP [DOC] FUNCTION | STEP-BODY &optional ARGS)"
  (let ((fn (car (last args)))
        (doc (when (= (length args) 2) (car args))))
    (if (functionp fn)
        ;; `buffer-file-name' is for the case evaluated interactively.
        (ecukes-steps-define body fn doc
                             (or load-file-name buffer-file-name))
      (ecukes-steps-call body args))))

;;;###autoload
(put 'ecukes-steps-define-or-call-step 'lisp-indent-function 'defun)
;;;###autoload
(put 'ecukes-steps-define-or-call-step 'doc-string-elt 2)

(defun ecukes-steps-define (body fn &optional doc file async)
  "Define step."
  (let ((body (ecukes-steps-format-body body))
        (arguments (ecukes-steps-body-arguments body)))
    (unless (-any?
             (lambda (step-def)
               (equal body step-def)) ecukes-steps-definitions)
      (add-to-list
       'ecukes-steps-definitions
       (make-ecukes-step-def :body body :fn fn :doc doc :file file
                             :args arguments :async async)))))

(defun ecukes-steps-body-arguments (body)
  "Retrieve argument names from BODY."
  (let ((continue t)
        (curr-point nil)
        (colon-point nil)
        (arg-num 0)
        (arg-names (list)))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (setq curr-point (point))
      (while (setq colon-point
                   (ecukes-parse-body-next-unescaped-backtick))
        (goto-char colon-point)
        (let ((colon-end-point
               (search-forward "'" (point-max) t)))
          (goto-char (+ colon-point 1))
          (setq arg-names
                (nconc arg-names
                       `(,(buffer-substring-no-properties
                           (point)
                           (- colon-end-point 1)))))))
      (when (equal (substring body -1) ":")
        (setq arg-names
              (nconc arg-names (list "pystring" "table"))))
      arg-names)))

(defun ecukes-steps-format-body (body)
  "Format body."
  (let ((continue t)
        (curr-point nil)
        (colon-point nil)
        (arg-num 0))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (setq curr-point (point))
      (while (setq colon-point
                   (ecukes-parse-body-next-unescaped-backtick))
        (goto-char colon-point)
        (let ((colon-end-point
               (search-forward "'" (point-max) t)))
          (goto-char colon-point)
          (delete-forward-char (- colon-end-point colon-point))
          (insert (format "`arg-%s'" (incf arg-num)))))
      (buffer-string))))

(defun ecukes-steps-format-step (body args)
  "Format step."
  (let ((continue t)
        (curr-point nil)
        (colon-point nil))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (setq curr-point (point))
      (while (setq colon-point
                   (ecukes-parse-body-next-unescaped-backtick))
        (goto-char colon-point)
        (let ((colon-end-point
               (search-forward "'" nil t)))
          (goto-char colon-point)
          (delete-forward-char (- colon-end-point colon-point))
          (insert (format "\"%s\"" (car args)))
          (setq args (cdr args))))
      (buffer-string))))

(defun ecukes-steps-call (body args)
  "Call step."
  (let* ((parsed-body nil)
         (step-def nil)
         (body body))
    (if (equal args '())
        (progn
          (setq parsed-body (ecukes-parse-body body))
          (setq body (car parsed-body))
          (setq args (cadr parsed-body)))
      (setq body (ecukes-steps-format-body body)))
    (setq step-def (ecukes-steps-find body))
    (if step-def
        (apply (ecukes-step-def-fn step-def)
               (or args
                   (ecukes-steps-args
                    (make-ecukes-step :body body))))
      (error (ansi-red "Step not defined: `%s`" body)))))

(defun ecukes-steps-without-definition (steps)
  "Return from STEPS those who have not been defined."
  (-reject
   (lambda (step)
     (ecukes-steps-find (ecukes-step-body step))) steps))

(defun ecukes-steps-find (name)
  "Find step by name."
  (-first
   (lambda (step-def)
     (equal (ecukes-step-def-body step-def) name))
   ecukes-steps-definitions))

(defun ecukes-steps-args (step)
  "Return args from step BODY."
  (ecukes-step-args step))

(provide 'ecukes-steps)

;;; ecukes-steps.el ends here
