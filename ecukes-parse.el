;;; ecukes-parse.el --- Simple line by line parser for feature files

(eval-when-compile (require 'cl))
(require 'dash)
(require 's)
(require 'ecukes-def)

(defconst ecukes-parse-intro-re
  "^\\s-*Feature:\\s-*\\(.+[^ ]\\)\\s-*$"
  "Regexp matching feature header.")

(defconst ecukes-parse-background-re
  "^\\s-*Background:"
  "Regexp matching background header.")

(defconst ecukes-parse-scenario-re
  "^[\t ]*Scenario:[\t ]*\\(.+?\\)[\t ]*$"
  "Regexp matching scenario header.")

(defconst ecukes-parse-outline-re
  "^[\t ]*Scenario Outline:[\t ]*\\(.+?\\)[\t ]*$"
  "Regexp matching scenario outline header.")

(defconst ecukes-parse-examples-re
  "^[\t ]*Examples:"
  "Regexp matching scenario outline examples header.")

(defconst ecukes-parse-step-re
  "^\\s-*\\(Given\\|When\\|Then\\|And\\|But\\)\\s-+\\(.+[^ ]\\)\\s-*$"
  "Regexp matching step.")

(defconst ecukes-parse-tags-re
  "^\\s-*@"
  "Regexp matching scenario tags.")

(defconst ecukes-parse-py-string-re
  "^\\s-*\"\"\""
  "Regexp matching py string.")

(defconst ecukes-parse-table-re
  "^\\s-*|.+|"
  "Regexp matching table.")


(defun ecukes-parse-feature (feature-file)
  "Parse FEATURE-FILE."
  (with-temp-buffer
    (insert (f-read-text feature-file 'utf-8))
    (goto-char (point-min))
    (let* ((tags)
           (intro (ecukes-parse-intro))
           (background (ecukes-parse-background))
           ;; (test (message "Test POINT!!!"))
           (outlines (ecukes-parse-outlines))
           (scenarios (append (ecukes-parse-scenarios) (-mapcat 'ecukes-generate-outlined-scenarios outlines))))
      (goto-char (point-min))
      (when (re-search-forward ecukes-parse-intro-re nil t)
        (setq tags (ecukes-parse-tags))
        (-map
         (lambda (scenario)
           (setf
            (ecukes-scenario-tags scenario)
            (-concat tags (ecukes-scenario-tags scenario))))
         scenarios))
      (make-ecukes-feature :intro intro :background background :outlines outlines :scenarios scenarios))))

(defun ecukes-parse-intro ()
  "Parse intro."
  (when (re-search-forward ecukes-parse-intro-re nil t)
    (let (description (header (match-string 1)))
      (while (not (progn (forward-line 1) (ecukes-parse-new-section-p)))
        (let ((line (ecukes-parse-line t)))
          (if line (push line description))))
      (make-ecukes-intro :header header :description (nreverse description)))))

(defun ecukes-parse-background ()
  "Parse background."
  (when (re-search-forward ecukes-parse-background-re nil t)
    (let ((steps (ecukes-parse-block-steps)))
      (make-ecukes-background :steps steps))))

(defun ecukes-parse-outlines ()
  "Parse all scenario outlines."
  (goto-char (point-min))
  (let (outlines)
    (while (re-search-forward ecukes-parse-outline-re nil t)
      (push (ecukes-parse-outline) outlines))
    (nreverse outlines)))

(defun ecukes-parse-outline ()
  "Parse a single scenario outline."
  (let ((name (ecukes-parse-outline-name))
        (tags (ecukes-parse-tags))
        (steps (ecukes-parse-block-steps))
        (table (ecukes-parse-outline-table)))
    (make-ecukes-outline :name name :tags tags :steps steps :table table)))

(defun ecukes-parse-outline-name ()
  "Parse scenario outline name."
  (save-excursion
    (let ((line (ecukes-parse-line)))
      (nth 1 (s-match ecukes-parse-outline-re line)))))

(defun ecukes-parse-outline-table ()
  "Parse examples table for a scenario outline."
  (save-excursion
    (catch 'table
      (let ((line (ecukes-parse-line)))
        (while (and (not (s-matches? ecukes-parse-examples-re (or line "")))
                    (not (ecukes-parse-new-section-p)))
          (forward-line 1)
          (setq line (ecukes-parse-line)))
        (when (s-matches? ecukes-parse-examples-re (or line ""))
          (throw 'table (ecukes-parse-table-step)))))))

(defun ecukes-substitute-in-steps (steps subs)
  (-map (lambda (step)
          (let ((gen  (copy-ecukes-step step))
                (type (ecukes-step-type step)))
            (setf (ecukes-step-name gen) (ecukes-substitute-in-string (ecukes-step-name gen) subs)
                  (ecukes-step-body gen) (ecukes-substitute-in-string (ecukes-step-body gen) subs))
            (cond
             ((eq type 'py-string)
              (setf (ecukes-step-arg gen) (ecukes-substitute-in-string (ecukes-step-arg gen) subs)))
             ((eq type 'table)
              (setf (ecukes-step-arg gen) (ecukes-substitute-in-table (ecukes-step-arg gen) subs))))
            gen))
        steps))

(defun ecukes-substitute-in-string (string subs)
  (let ((new-s (copy-sequence string))
        (reps  (copy-sequence subs)))
    (while (not (zerop (length reps)))
      (setq new-s (s-replace (format "<%s>" (car reps)) (cadr reps) new-s))
      (setq reps (cddr reps)))
    new-s))

(defun ecukes-substitute-in-table (table subs)
  (-map (lambda (row)
          (-map (lambda (cell) (ecukes-substitute-in-string cell subs)) row))
        table))

(defun ecukes-generate-outlined-scenarios (outline)
  "Generate scenarios from an outline."
  (let* ((name  (ecukes-outline-name outline))
         (steps (ecukes-outline-steps outline))
         (tags  (ecukes-outline-tags outline))
         (table (ecukes-outline-table outline))
         (header (car table)))
    (-map (lambda (row)
            (make-ecukes-scenario :name name :tags tags :steps (ecukes-substitute-in-steps steps (-interleave header row))))
          (cdr table))))

(defun ecukes-parse-scenarios ()
  "Parse scenarios."
  (goto-char (point-min))
  (let (scenarios)
    (while (re-search-forward ecukes-parse-scenario-re nil t)
      (push (ecukes-parse-scenario) scenarios))
    (nreverse scenarios)))

(defun ecukes-parse-scenario ()
  "Parse scenario."
  (let ((name (ecukes-parse-scenario-name))
        (tags (ecukes-parse-tags))
        (steps (ecukes-parse-block-steps)))
    (make-ecukes-scenario :name name :steps steps :tags tags)))

(defun ecukes-parse-scenario-name ()
  "Parse scenario name."
  (save-excursion
    (let ((line (ecukes-parse-line)))
      (nth 1 (s-match ecukes-parse-scenario-re line)))))

(defun ecukes-parse-tags ()
  "Parse tags."
  (save-excursion
    (forward-line -1)
    (let ((line (ecukes-parse-line t)))
      (when (and line (s-matches? ecukes-parse-tags-re line))
        (-distinct
         (-map
          (lambda (tag)
            (substring tag 1))
          (split-string line "\\s-+")))))))

(defun ecukes-parse-block-steps ()
  "Parse steps in block."
  (let (steps)
    (while (ecukes-forward-step)
      (let ((step (ecukes-parse-step)))
        (push step steps)))
    (nreverse steps)))

(defun ecukes-parse-step ()
  "Parse step."
  (let* ((name (ecukes-parse-line t))
         (matches (s-match ecukes-parse-step-re name))
         (head (nth 1 matches))
         (body (nth 2 matches))
         (parsed-body)
         (arg)
         (params)
         (type))
    (cond
     ((ecukes-parse-py-string-step-p)
      (setq arg (ecukes-parse-py-string-step))
      (setq type 'py-string))
     ((ecukes-parse-table-step-p)
      (setq arg (ecukes-parse-table-step))
      (setq type 'table))
     (t (setq type 'regular)
        (setq parsed-body (ecukes-parse-body body))
        (setq body (car parsed-body))
        (setq params (cadr parsed-body))))
    (make-ecukes-step :name name :head head :body body 
                      :type type :arg arg :params params)))

(defun ecukes-char-at-point ()
  (buffer-substring-no-properties (point) (+ (point) 1)))

(defun ecukes-parse-body-find-unescaped-quote (quote)
  (let ((quote-point nil)
        (continue t)
        (continue-looking t)
        (escaped-quote nil))
    (while (and continue
                (setq quote-point
                      (search-forward quote (point-max) t)))
      (setq escaped-quote nil)
      (when (> (point) 2)
        (backward-char 2)
        (while (and continue-looking 
                    (equal (ecukes-char-at-point) "\\"))
          (if (null escaped-quote)
              (setq escaped-quote t)
            (setq escaped-quote nil))
          (if (> (point) 1)
              (backward-char 1)
            (setq continue-looking nil))))
      (if (null escaped-quote)
          (setq continue nil)
        (progn
          (goto-char quote-point)
          (setq quote-point nil))))
    (when quote-point
      (- quote-point 1))))

(defun ecukes-parse-body-next-unescaped-single-quote ()
  (ecukes-parse-body-find-unescaped-quote "'"))

(defun ecukes-parse-body-next-unescaped-double-quote ()
  (ecukes-parse-body-find-unescaped-quote "\""))

(defun ecukes-parse-body-next-unescaped-quote ()
  (let ((curr-point (point))
        (double-quote-pos nil)
        (single-quote-pos nil))
    (setq double-quote-pos
         (ecukes-parse-body-next-unescaped-double-quote))
    (goto-char curr-point)
    (setq single-quote-pos
         (ecukes-parse-body-next-unescaped-single-quote))
    (if (and (not (null double-quote-pos))
             (not (null single-quote-pos)))
        (if (< double-quote-pos single-quote-pos)
          double-quote-pos
          single-quote-pos)
      (if (not (null double-quote-pos))
          double-quote-pos
        (when (not (null single-quote-pos))
          single-quote-pos)))))

(defun ecukes-parse-body (body)
  (let ((parameters '())
        (continue t)
        (curr-point nil)
        (quote-point nil))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (setq curr-point (point))
      (while (setq quote-point
                   (ecukes-parse-body-next-unescaped-quote))
        (goto-char quote-point)
        (if (equal (ecukes-char-at-point) "'")
            (let* ((symbol-end-point
                    (search-forward " " (point-max) t)))
              (if (null symbol-end-point)
                  (setq symbol-end-point
                        (point-max))
                (setq symbol-end-point
                      (- symbol-end-point 1)))
              (setq symbol-content
                    (buffer-substring-no-properties
                     quote-point
                     symbol-end-point))
              (add-to-list 'parameters
                           (substring symbol-content 1) t)
              (goto-char quote-point)
              (delete-forward-char (length symbol-content))
              (insert ":param"))
          (when (equal (ecukes-char-at-point) "\"")
            (let ((start-dbl-quote quote-point)
                  (end-dbl-quote nil)
                  (string-content ""))
              (goto-char (+ start-dbl-quote 1))
              (setq end-dbl-quote
                    (ecukes-parse-body-next-unescaped-double-quote))
              (setq string-content
                    (buffer-substring-no-properties
                     start-dbl-quote
                     end-dbl-quote))
              (add-to-list 'parameters
                           (substring string-content 1)
                           t)
              (goto-char start-dbl-quote)
              (delete-forward-char (+ (length string-content) 1))
              (insert ":param")))))
      (list (buffer-string) parameters))))

(defun ecukes-parse-table-step-p ()
  "Check if step is a table step or not."
  (save-excursion
    (forward-line 1)
    (let ((line (ecukes-parse-line)))
      (s-matches? ecukes-parse-table-re line))))

(defun ecukes-parse-table-step ()
  "Parse table step."
  (save-excursion
    (forward-line 1)
    (let (rows)
      (while (s-matches? ecukes-parse-table-re (ecukes-parse-line))
        (push (ecukes-parse-table-step-row) rows)
        (forward-line 1))
      (nreverse rows))))

(defun ecukes-parse-table-step-row ()
  "Parse row in table."
  (let ((row (ecukes-parse-line)))
    (butlast (cdr (split-string row "\\s-*|\\s-*")))))

(defun ecukes-parse-py-string-step-p ()
  "Check if step is a py string step or not."
  (save-excursion
    (forward-line 1)
    (let ((line (ecukes-parse-line)))
      (s-matches? ecukes-parse-py-string-re line))))

(defun ecukes-parse-py-string-step ()
  "Parse py string step."
    (forward-line 1)
    (let ((whites
           (save-excursion
             (back-to-indentation)
             (current-column)))
          (lines))
      (forward-line 1)
      (while (not (s-matches? ecukes-parse-py-string-re (ecukes-parse-line)))
        (let ((line (ecukes-parse-line)))
          (push (if (<= whites (length line)) (substring line whites) nil) lines))
        (forward-line 1))
      (s-join "\n" (nreverse lines))))

(defun ecukes-parse-line (&optional strip-whitespace)
  "Parse current line."
  (let* ((raw (buffer-substring (line-beginning-position) (line-end-position)))
         (line (if strip-whitespace (s-trim raw) raw)))
    (if (and strip-whitespace (equal line "")) nil line)))

(defun ecukes-forward-step ()
  "Go one step forward within current section."
  (forward-line 1)
  (let ((line (ecukes-parse-line t)))
    (unless (ecukes-parse-new-section-p)
      (if (s-matches? ecukes-parse-step-re (or line ""))
          (not (not line))
        (ecukes-forward-step)))))

(defun ecukes-parse-new-section-p ()
  "Check if current line is the start of a new section."
  (let ((line (or (ecukes-parse-line t) "")))
    (or
     (eobp)
     (s-matches? ecukes-parse-background-re line)
     (s-matches? ecukes-parse-outline-re line)
     (s-matches? ecukes-parse-examples-re line)
     (s-matches? ecukes-parse-scenario-re line)
     (s-matches? ecukes-parse-tags-re line))))


(provide 'ecukes-parse)

;;; ecukes-parse.el ends here
