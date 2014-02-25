(defun ecukes-ansi-clear (string)
  "Like `ansi-color-filter-apply' with extension for movement."
  (ansi-color-filter-apply
   (->> string
     (replace-regexp-in-string "\n\u001b\\[[0-9]+A" "")
     (replace-regexp-in-string "\u001b\\[[0-9]+[BCD]" ""))))

(defun ecukes-should-match (needle haystack)
  (should (s-contains? needle haystack)))

(define-step "I run ecukes `command'"
  (let* ((buffer-name "*ecukes-output*")
         (buffer
          (progn
            (when (get-buffer buffer-name)
              (kill-buffer buffer-name))
            (get-buffer-create buffer-name)))
         (default-directory (file-name-as-directory ecukes-project-path))
         (args
          (unless (equal command "")
            (s-split " " command)))
         (exit-code
          (apply
           'call-process
           (append (list ecukes-executable nil buffer nil) args))))
    (with-current-buffer buffer
      (let ((content (ecukes-ansi-clear (buffer-string))))
        (cond ((= exit-code 0)
               (setq ecukes-stdout content))
              (t
               (setq ecukes-stderr content)))))))

(define-step "I should see command output:"
    (ecukes-should-match pystring ecukes-stdout))

(define-step "I should see command error:"
  (ecukes-should-match pystring ecukes-stderr))

(define-step "feature `name':"
  (let ((dirs (-reject 's-blank? (s-split "/" (f-parent name))))
        (path (f-expand (s-concat name ".feature") ecukes-project-features-path))
        (default-directory ecukes-project-features-path))
    (unless (f-dir? (f-parent name))
      (apply 'f-mkdir dirs))
    (f-write-text (s-concat pystring "\n") 'utf-8 path)))

(define-step "step definition:"
  (let ((path (f-expand "super-project-steps.el" ecukes-project-step-definitions-path)))
    (f-write-text (s-concat pystring "\n") 'utf-8 path)))

(define-step "these files should exist:"
  (let ((files (cdr table)))
    (-each
        files
      (lambda (file)
        (should (f-exists? (f-expand (car file) ecukes-project-path)))))))

(define-step "I visit project `name'"
  (setq ecukes-project-path (f-expand name ecukes-projects-path))
  (unless (f-dir? ecukes-project-path)
      (f-mkdir ecukes-project-path)))

(define-step "the file `file' should contain:"
  (ecukes-should-match pystring (f-read-text (f-expand file ecukes-project-path) 'utf-8)))

(define-step "I should see list of reporters:"
  (lambda ()
    (-each
     '("dot" "spec" "landing" "progress" "magnars" "gangsta")
     (lambda (reporter)
       (Then "I should see command output:" (s-concat reporter " - "))))))

(define-step "I create file `file' with content:"
  (let ((path (f-expand file ecukes-project-path)))
    (f-write-text pystring 'utf-8 path)))

(define-step "I byte compile `file'"
  (byte-compile-file (f-expand file ecukes-project-path)))
