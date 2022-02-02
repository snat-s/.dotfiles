;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(setq n ?A)
(setq current-round "~/progra/competitiva_2/CodeForces/Educational/#122/")


(defun competitive-programming-create-base-files ()
  "Create the A-Z files in competitive programming"
  (interactive)
  (create-empty-makefile)
  (dotimes (i 5)
    (setq current-file (concat (char-to-string n) ".cpp"))
    (make-empty-file (concat current-round current-file))
    (append-to-current-file current-file)
    (append-current-make-option current-file n)
    (message "%c %s"  n current-file)
    (setq n (+ 1 n))))

(defun create-empty-makefile ()
  (make-empty-file (concat current-round "Makefile")))

(defun append-current-make-option (cf n)
  "Appends to the make-file the current file that is beign processed
   cf => Current File
   n => letter of the file"
  (write-region (concat (char-to-string n) ":\n\t") nil (concat current-round "Makefile") 'append)
  (write-region (concat "g++ " cf " -o " (char-to-string n) " -g -Wall\n") nil (concat current-round "Makefile") 'append))

(defun append-to-current-file (cf)
  "cf => Current File
   Appends the main in cp to the current file"
  (write-region "#include <iostream>\n\nint main(void) {\n\tstd::cin.tie(0);\n\tstd::ios_base::sync_with_stdio(0);\n\n\treturn 0;\n}" nil (concat current-round cf) 'append))

(defun cp-create-programming-view ()
  (interactive)
  (split-window-right 130)
  (split-window-below -10)
  (ibuffer-other-window))
