;; emacs support for rsltc tool
;; Chris George, UNU/IIST, March 1999
;; Based heavily on compile.el

(require 'compile)

;; include directory where rsltc is located if not on path
(defvar rsltc-command
  (if (eq system-type 'windows-nt)
      "C:\\raise\\rsl\\rsltc"
    "rsltc")
  "RSL type checker")

;; local copy of shell file name (for reverting after using cygwin in windows)
(defvar rsltc-shell-file-name
  shell-file-name
  "Copy of shell-file-name")

;; set this to vcg command (probably no "x" in Windows, and may need path)
;; like "c:\\raise\\vcg\\bin\\vcg"
(defvar vcg-command
  (if (eq system-type 'windows-nt)
      "C:\\raise\\vcg\\bin\\vcg"
    "xvcg")
  "Command to run vcg")

;; where SAL make file and prelude files are stored
(defvar rsltc-sal-directory 
  (if (eq system-type 'windows-nt)
      "C:\\Cygwin\\usr\\share\\rsltc\\sal"
    "/usr/share/rsltc/sal")
  "SAL make and prelude file folder")

(defvar rsltc-sal-directory-unix
  "/usr/share/rsltc/sal"
  "SAL make and prelude file folder for Linux and Cygwin")

(defvar ltl_script
  "ltl_script.sh"
  "Script for converting ltl expressions to fdr test processes")

(defvar RSL-entity-regexp "\\<\\(scheme\\|object\\|theory\\|devt_relation\\|hint\\)\\>"
  "Regular expression for keyword starting an RSL entity.")

(defvar RCS-regexp "[ \t]*--[ \t]*\\$Id:"
  "Regular expression for start of RCS data.")

(defvar rsl-line-comment-regexp "[ \t]*--"
  "Start of line comment")

(defvar rsl-block-comment-start-regexp "/\\*"
  "Start of block comment")

(defvar rsl-block-comment-end-regexp "\\*/"
  "End of block comment")

(defvar rsl-context-regexp "[-a-zA-Z0-9_./]+"
  "Context entry.")

(defvar RSL-basename nil
  "Base name of RSL file")

(defvar RSL-process nil
  "Process created by rsl command, or nil if none exists now.
Note that the process may have been \"deleted\" and still
be the value of this variable.")

(defvar RSL-buffer nil
  "Buffer containing RSL.")

(defvar rsltc-error-regexp
  "^\\([a-zA-Z][^.]*\\.rsl\\):\\([0-9]+\\):\\([0-9]+\\):"
  "Regular expression for file, line and column in rsltc error output.")

(defun rsltc (params)
  "Run RSL type checker"
  (interactive "sCommand line arguments: ")
  (if (buffer-modified-p) (save-buffer))
  ;; in case have been switched to cygwin in Windows
  (setq shell-file-name rsltc-shell-file-name)
  (setq RSL-basename
	(file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
  (compile (concat rsltc-command " " params " " RSL-basename)))

(defun rslpp (len-str)
  "Run RSL pretty printer on file in current buffer"
  (interactive "sLine length (default 60): ")
  (let ((len
	 (if (string-equal len-str "") 60 (string-to-int len-str))))
    (delete-other-windows) ; prevent old error messages still being visible
    (rsl-do
     (format "%s -p -pl %d " rsltc-command len)
     'end-pp-ok)))

(defun end-pp-ok (buff)
  (save-excursion
    (set-buffer buff)
    (copy-to-buffer RSL-buffer (point-min) (point-max))
    (message "Pretty printed: C-x u to undo")))

(defun rslgg ()
  "Make dependency graph of file in current buffer and display"
  (interactive)
  (rsl-do
   (concat rsltc-command " -g ")
   'end-gg-ok))

(defun end-gg-ok (buff)
  (call-process
   shell-file-name nil 0 nil "-c"
   (concat vcg-command " " RSL-basename ".vcg")))

(defun rsltc-and-sml ()
  "Translate to sml and run sml if no errors"
  (interactive)
  (rsl-do
   (concat rsltc-command " -m ")
   'end-sml-ok))

(defun end-sml-ok (buff)
  (rsl-sml-load))

(defun rsl-do (command post-fun)
  "Run COMMAND (which must end with a space) on RSL-basename
and if successful then run post-fun with shell output buffer name as argument."
  (if (buffer-modified-p) (save-buffer))
  (setq RSL-basename
	(file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
  (setq RSL-buffer (current-buffer))
  (if (get-buffer " *rslbuff*") (kill-buffer " *rslbuff*"))
  ;; in case have been switched to cygwin in Windows
  (setq shell-file-name rsltc-shell-file-name)
  (let ((status (call-process
		 shell-file-name nil
		 (get-buffer-create " *rslbuff*") nil "-c"
		 (concat command RSL-basename))))
    (if (and (numberp status)
	     (or (zerop status)
		 ;; strange status returned with Cygwin bash
		 (and (string= shell-file-name "bash")
		      (= status 131072))))
	(apply post-fun (list " *rslbuff*"))
      (end-RSL-error " *rslbuff*"))))

(defun end-RSL-error (buff)
  (set-buffer buff)
  (goto-char (point-min))
  (if (re-search-forward rsltc-error-regexp nil t)
      ;; Extract line and column number from error message.
      (let
	  ((filename
	    (buffer-substring (match-beginning 1) (match-end 1)))
	   (linenum
	    (string-to-int
	     (buffer-substring (match-beginning 2) (match-end 2))))
	   (colnum
	    (string-to-int
	     (buffer-substring (match-beginning 3) (match-end 3)))))
	;; Go to RSL buffer and find the erring line.
	(set-buffer RSL-buffer)
	(if (not (string-equal (buffer-file-name) filename))
	    (find-file filename))
	(goto-char (point-min))
	(forward-line (1- linenum))
	(forward-char (1- colnum))
	(message
	 (format "Error here: %s: line %d, column %d" filename linenum colnum)))
    ;; something else
    ;; just show rsltc output
    (switch-to-buffer-other-window buff)))

;; support error reports in RSL comments 
(setq compilation-error-regexp-alist
      (cons
       '(
	 "[ \t]*(*-- \\([a-zA-Z0-9_/.]+\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)
	    compilation-error-regexp-alist))

;; additions for emacs-based mkdoc ------------------

(defun my-do-rsl (name command file) (do-rsl))

(defun my-undo-rsl (name command file) (undo-rsl))

(defun my-do-latex (name command file) (do-latex))

(defun my-undo-latex (name command file) (undo-latex))

(defun my-mkdoc (name command file) (mkdoc))

(defun my-TeX-run-LaTeX (name command file)
  "Call save-some-buffers before calling latex to remind people to save files"
  (progn
    (save-some-buffers)
    (TeX-run-LaTeX name command file)))

(defun my-TeX-run-interactive (name command file)
  "Call save-some-buffers before calling latex to remind people to save files"
  (progn
    (save-some-buffers)
    ;; in version 20.3 at least, need to deactivate temp-buffer-setup-hook
    ;; else output buffer is in help mode, which is read only
    ;; CWG 2/11/99
    (if (boundp 'temp-buffer-setup-hook)
	(let ((save-hook temp-buffer-setup-hook))
	  (setq temp-buffer-setup-hook nil)
	  (TeX-run-interactive name command file)
	  (setq temp-buffer-setup-hook save-hook))
      (TeX-run-interactive name command file))))

(defun my-TeX-run-command (name command file)
  "Call save-some-buffers before calling command to remind people to save files"
  (progn
    (save-some-buffers)
    (TeX-run-command name command file)))

(defun mkdoc ()
  "Creates .tex files from \RAISEIN, \RAISEINFRAME, and \RAISEINBOX commands"
  (interactive)
  (let ((save-case-fold-search case-fold-search)
	(save-point (point)))
    (goto-char (point-min))
    (setq case-fold-search nil)
    (while (re-search-forward
	    "\\\\\\(RAISEIN\\|RAISEINFRAME{[^}]*}\\|RAISEINBOX\\){\\([^}]*\\)}" nil t)
      (let ((rslfile (concat (buffer-substring (match-beginning 2) (match-end 2)) ".rsl"))
	    (texfile (concat (buffer-substring (match-beginning 2) (match-end 2)) ".tex")))
	(if (file-readable-p rslfile)
	    (if (not (up-to-date texfile rslfile))
		(save-excursion
		  ;; uses a special buffer with a leading space in name
		  ;; to avoid font-lock starting and slowing things down
		  (if (get-buffer " *rslbuff*") (kill-buffer " *rslbuff*"))
		  (setq rslfile (expand-file-name rslfile))
		  (setq texfile (expand-file-name texfile))
		  (let ((texbuffer (get-buffer-create " *rslbuff*")))
		    (set-buffer texbuffer)
		    (insert-file rslfile)
		    (goto-char (point-min))
		    (insert "\\RSLatex\n")
		    (setq case-fold-search nil)
		    (mark-context)
		    (goto-char (point-max))
		    (backward-char 1)
		    (if (looking-at "\n")
			(forward-char 1)
		      (progn
			(forward-char 1)
			(insert "\n")))
		    (insert "\\endRSLatex\n")
		    (do-latex)
		    (write-region (point-min) (point-max) texfile)
		    (kill-buffer texbuffer))))
	  (progn
	    (setq case-fold-search save-case-fold-search)
	    (error "RSL file %s cannot be found" rslfile)))))
    (setq case-fold-search save-case-fold-search)
    (goto-char save-point))
  (message "mkdoc completed"))

(defun up-to-date (texfile rslfile)
  "Returns t if TEXFILE exists and modified no earlier than RSLFILE."
  (and (file-readable-p texfile)
      (let ((tex-mod-time (nth 5 (file-attributes texfile)))
	    (rsl-mod-time (nth 5 (file-attributes rslfile))))
	(or (> (car tex-mod-time) (car rsl-mod-time))
	    (and (= (car tex-mod-time) (car rsl-mod-time))
		 (>= (car (cdr tex-mod-time)) (car (cdr rsl-mod-time))))))))

(defun mark-context ()
  (re-search-forward
   (concat rsl-block-comment-start-regexp "\\|"
	   rsl-line-comment-regexp "\\|" rsl-context-regexp) nil t)
  (if (string-match rsl-block-comment-start-regexp
		    (buffer-substring (match-beginning 0) (match-end 0)))
      (progn
	(skip-block-comment 1)
	(mark-context))
    (beginning-of-line)
    (if (looking-at rsl-line-comment-regexp)
	(progn
	  (if (looking-at RCS-regexp)
	      ;; do not include RCS data
	      (let ((start (point)))
		(search-forward "$" nil nil 2)
		(forward-line 1)
		(delete-region start (point)))
	    (forward-line 1))
	  (mark-context))
      (if (not (looking-at (concat "[ \t]*" RSL-entity-regexp)))
	  (let ((end-found nil)
		(start-context (point))
		end-context)
	    (insert "\\CONTTWO{")
	    (re-search-forward "[^ \t]")
	    (while (not end-found)
	      (re-search-forward "[ \t\n]+" nil t)
	      (let ((p1 (match-beginning 0))
		    (p2 (match-end 0)))
		(if (looking-at ",") ()
		  (goto-char (1- p1))
		  (setq end-found (not (looking-at ",")))
		  (goto-char p2))))
	    (backward-char 1)
	    (insert "}")
	    (setq end-context (point))
	    (goto-char start-context)
	    (while (search-forward "_" (1+ end-context) 1)
	      (replace-match "\\\\_")))))))
	    

(defun skip-block-comment (count)
  (while (> count 0)
    (if (re-search-forward
	 (concat rsl-block-comment-start-regexp "\\|"
		 rsl-block-comment-end-regexp) nil t)
	(progn
	  (goto-char (match-beginning 0))
	  (if (looking-at rsl-block-comment-start-regexp)
	      (setq count (1+ count))
	    (setq count (1- count)))
	  (forward-char 2))
      (error "Perhaps opening comment not closed")))
  (if (looking-at rsl-line-comment-regexp)
      (forward-line)))

;; menu functions needed for rsl-mode
(defun rsltc-only () ""
  (interactive)
  (rsltc ""))

(defun rslpp-dflt () ""
  (interactive)
  (rslpp ""))

(defun rsltc-c () ""
  (interactive)
  (rsltc "-c"))
    
(defun rsltc-cc () ""
  (interactive)
  (rsltc "-cc"))
    
(defun rsltc-d () ""
  (interactive)
  (rsltc "-d"))
    
(defun rsltc-m () ""
  (interactive)
  (rsltc "-m"))
    
;; run sml after translation to sml
(defun rsltc-m-and-run () ""
  (interactive)
  (rsltc-and-sml))
    
(defun rsltc-cpp () ""
  (interactive)
  (rsltc "-c++"))
    
(defun rsltc-cppv () ""
  (interactive)
  (rsltc "-cpp"))
    
(defun rsltc-sql () ""
  (interactive)
  (rsltc "-c++ -sql"))

(defun rsltc-sqlv () ""
  (interactive)
  (rsltc "-cpp -sql"))

(defun rsltc-pvs () ""
  (interactive)
  (rsltc "-pvs"))

(defun rsltc-pc () ""
  (interactive)
  (rsltc "-pc"))

(defun rsltc-sal () ""
  (interactive)
  (rsltc "-sal"))

(defun rsltc-csp () ""
  (interactive)
  (rsl-do
   (concat rsltc-command " -f ")
   'rsltc-ltl-csp))

(defun rsltc-ltl-csp (buff)
  (let* ((rslfile (buffer-file-name))
	 (basename
	  (file-name-sans-extension (file-name-nondirectory rslfile))))
    (apply 'call-process ltl_script nil 0 nil (list basename))))	 



(defun rsl-has-new-sml-file ()
  (and
   (fboundp 'sml)
   (let* ((rslfile (buffer-file-name))
	  (smlfile (concat (file-name-sans-extension rslfile) ".sml")))
     ; considered new enough if rsl file is not newer
     ; to avoid problems when times not distinguishable
     (not (file-newer-than-file-p rslfile smlfile)))))
    
(defun rsl-sml-load ()
  ;; based on sml-load-file
  (interactive)
  (if (get-buffer "*sml*")
      (rsl-sml-results))
  (if (fboundp 'sml)
      (let ((file (concat
		   (file-name-sans-extension
		    (buffer-file-name))
		   ".sml")))
	(if (file-readable-p file)
	    (progn
	      (save-excursion (sml))
	      (setq sml-prev-l/c-dir/file
		    (cons (file-name-directory file) (file-name-nondirectory file)))
	      (sml-update-cursor (sml-proc-buffer))
	      (comint-send-string
	       (sml-proc) (concat (format sml-use-command file) ";\n"))
	      (switch-to-sml nil))
	  (error "SML file %s does not exist" file)))
    (error "You must have SML available")))


(defun rsl-have-sml-buff ()
  (get-buffer "*sml*"))

;; ----------------------------------------------------
;; support for mutation testing 
;; ----------------------------------------------------

(defvar delete-dir-command "rm -rf")
(defvar copy-command "cp")
(defvar compare-command "cmp")

(defvar rsl-mutant-base-dir nil "Base directory from which mutants are being created")

(defvar rsl-test-case-file nil "Name of last test case file - used as default on next choice.")

(defvar coverage-message-string
  "^\\(Unexecuted expressions in \\|Complete expression coverage of \\)"
  "Regexp matching strings before RSL file name reporting on coverage in SML output.

Must match strings defined in rslml.sml")

(defun rsl-sml-results ()
  "Saves test case results from last run of sml and kills *sml* buffer."
  (interactive)
  (let ((res-file nil))
    (if (get-buffer "*sml*")
	(save-excursion
	  (set-buffer "*sml*")
	  (if (file-accessible-directory-p default-directory)
	      (progn
		(goto-char (point-max))
		(if (search-backward "<sig>" nil t)
		    (if (search-forward "open " nil t)
			(let ((beg (point))
			      (module)
			      (cover-file))
			  (end-of-line)
			  (setq module (buffer-substring beg (point)))
			  (setq res-file (format "%s.sml.results" module))
			  (if (get-buffer res-file)
			      (kill-buffer res-file))
			  (forward-line 2)
			  (delete-region (point-min) (point))
			  (delete-matching-lines "^val it = () : unit"
						 (point-min) (point-max))
			  (delete-matching-lines "^- *$"
						 (point-min) (point-max))
			  (goto-char (point-min))
			  (while (re-search-forward coverage-message-string nil t)
			    (progn
			      (setq beg (point))
			      (end-of-line)
			      (setq cover-file
				    (concat (buffer-substring beg (point)) ".el"))
			      (rsl-save-coverage cover-file 0)))
			  (delete-matching-lines coverage-message-string
						 (point-min) (point-max)) 
			  
					; avoid getting backups or previous results
			  (if (file-exists-p res-file) (delete-file res-file))
			  (write-file res-file nil)
			  (message (format "SML results file %s written."
					   res-file))
			  (setq res-file (expand-file-name res-file)))
		      (message "Has sml finished running?"))
		  (message "Has sml finished running?"))))
	  (kill-buffer nil)
	  (if res-file (find-file-other-window res-file)))
      (message "No sml buffer."))))

(defun rsl-last-mutant ()
  "Checks for dirs mutant0, ... and returns first n for which mutantn+1 does not exist; or nil if none exist."
  (rsl-last-mutant1 0))

(defun rsl-last-mutant1 (num)
  (if rsl-mutant-base-dir
      (let* ((numstr (int-to-string num))
	     (dir (concat "mutant" numstr)))
	(if (file-accessible-directory-p dir)
	    (rsl-last-mutant1 (1+ num))
	  (if (> num 0) (int-to-string (1- num)) nil)))
    nil))

(defun make-mutant-dir (num)
  "Create directory mutantn, choosing n so directory is new."
  (let* ((numstr (int-to-string num))
	 (dir (concat "mutant" numstr)))
    (if (file-accessible-directory-p dir)
	(make-mutant-dir (1+ num))
      (make-directory dir)
      dir)))

(defun has-mutant-dirs ()
  (directory-files default-directory nil "mutant[0-9]+"))

(defun is-mutant-dir ()
  (string-match "mutant[0-9]+/$" default-directory))

(defun kill-deleted-file-buffer (buff)
  (save-excursion
    (set-buffer buff)
    (let ((file (buffer-file-name)))
      (if (and file
	       (string-match "mutant[0-9]+/" (expand-file-name file))
	       (not (file-exists-p file)))
	  (kill-buffer nil)))))

(defun kill-deleted-file-buffers ()
  "Used after deleting mutant directories to kill
any buffers visiting deleted files in mutant directories."
  (let ((bufflist (buffer-list)))
    (while bufflist
      (kill-deleted-file-buffer (car bufflist))
      (setq bufflist (cdr bufflist)))))

(defun kill-deleted-dired-buffers ()
  (let ((alist dired-buffers) elt dir buf)
    (while alist
      (setq elt (car alist)
	    dir (car elt)
	    buf (cdr elt))
      (if (and
	   (string-match "mutant[0-9]+/" dir)
	   (not (file-directory-p dir)))
	  (kill-buffer buf))
      (setq alist (cdr alist)))))
      

(defun rsl-make-mutate ()
  "copy RSL files to directory mutantn, choosing n so directory is new.

In copy of current buffer replace string marked by beg end with str."
  (interactive)
  (let ((beg (region-beginning))
	(end (region-end))
	(str (read-string "Replacement string: "))
	(mudir (make-mutant-dir 0))
	(current (file-name-nondirectory (buffer-file-name)))
	(testfile))
    (shell-command (format "%s *.rsl %s" copy-command mudir))
    (save-excursion
      (find-file (concat mudir "/" current))
      (goto-char beg)
      (delete-region beg end)
      (insert str)
      (save-buffer)
      (kill-buffer nil))
    (setq testfile
	  (read-file-name
	   "RSL test case file: "
	   ""
	   (or rsl-test-case-file current)
	   t
	   (or rsl-test-case-file current)))
    (let ((newfile (concat mudir "/" testfile)))
      (if (file-exists-p newfile)
	  (progn
	    (setq rsl-test-case-file testfile)
	    (find-file newfile))
	(message (format "Cannot open test case file %s" newfile)))
      mudir)))

(defun rsl-mutate ()
  (interactive)
  (if (get-buffer "*sml*")
      (rsl-sml-results))
  (setq rsl-mutant-base-dir default-directory)
  (rsl-make-mutate)
  (rsltc-m-and-run))

  
(defun rsl-delete-mutant-dirs1 (num)
  (let* ((numstr (int-to-string num))
	 (dir (concat "mutant" numstr)))
    (if (file-accessible-directory-p dir)
	(progn
	  (shell-command (format "%s %s" delete-dir-command dir))
	  (rsl-delete-mutant-dirs1 (1+ num))))))

(defun rsl-delete-mutant-dirs ()
  "Deletes directories mutant0, mutant1 etc.
Also kills buffers visiting files in deleted mutant directories."
  (interactive)
  (rsl-delete-mutant-dirs1 0)
  (kill-deleted-file-buffers)
  (kill-deleted-dired-buffers))

(defun rsl-base-results-file ()
  (if (and rsl-test-case-file rsl-mutant-base-dir)
      (let* ((file-base (file-name-sans-extension rsl-test-case-file))
	     (res-file (concat rsl-mutant-base-dir file-base ".sml.results")))
	(if (file-exists-p res-file)
	    res-file
	  ""))
    ""))

(defun rsl-mutant-results-file (num-string)
  (if (and rsl-mutant-base-dir
	   rsl-test-case-file)
      (let* ((file-base (file-name-sans-extension rsl-test-case-file))
	     (res-file (concat
			rsl-mutant-base-dir
			"mutant"
			num-string
			"/"
			file-base ".sml.results")))
	(if (file-exists-p res-file)
	    res-file
	  ""))
    ""))

(defun rsl-compare-with-mutant ()
  (interactive)
  (if (get-buffer "*sml*")
      (rsl-sml-results))
  (let ((filea (rsl-base-results-file))
	(fileb "")
	(numb-string nil)
	(save-use-dialog-box (if (boundp use-dialog-box) use-dialog-box nil)))
    (if (string-match "/mutant\\([0-9]+\\)/$" default-directory)
	(setq numb-string
	      (substring default-directory (match-beginning 1) (match-end 1)))
      (setq numb-string (rsl-last-mutant)))
    ;; use minibuffer rather than dialog box to get file names
    ;; (affects Windows but not Linux)
    (if (boundp use-dialog-box) (setq use-dialog-box nil))
    (setq filea
	  (read-file-name
	   "Base results file: "
	   ""
	   filea
	   t
	   filea))
    (if numb-string (setq fileb (rsl-mutant-results-file numb-string)))
    (setq fileb
	  (read-file-name
	   "Mutant results file: "
	   ""
	   fileb
	   t
	   fileb))
    (if (boundp use-dialog-box) (setq use-dialog-box save-use-dialog-box))
    (ediff-files filea fileb))) 

;; -------------------------------------------------------
;; support for coverage testing
;; -------------------------------------------------------

(defvar rsl-overlays nil)

(defvar rsl-overlay-poss nil) 

(defun rsl-lc-to-pos (line col)
  "Convert line and column to point position."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char (1- col))
    (point)))

(defun rsl-clear-overlays ()
  (while rsl-overlays
    (delete-overlay (car rsl-overlays))
    (setq rsl-overlays (cdr rsl-overlays))))
      
(defun rsl-make-overlays ()
  (rsl-clear-overlays)
  (rsl-make-overlays1 rsl-overlay-poss)
  (rsl-show-overlays))

(defun rsl-reduce-finish (position)
  "Shift left over keyword and then whitespace and/or punctuation"
  (save-excursion
    (goto-char position)
    (while (get-text-property (point) 'face)
      (backward-char 1))
    (skip-syntax-backward " .>b")
    (rsl-skip-comments-backward)
    (point)))

(defun rsl-skip-comments-backward ()
  (while (equal (get-text-property (1- (point)) 'face)
	       'font-lock-comment-face)
    (backward-char 1))
  (if (< (skip-syntax-backward " .>b") 0)
      (rsl-skip-comments-backward)))

(defun rsl-make-overlays1 (poss)
  (if (consp poss)
      (let* ((pos (car poss))
	     (beg (car pos))
	     (fin (car (cdr pos)))
	     (fin1 (rsl-reduce-finish fin))
	     (overlay (make-overlay beg fin1)))
	(setq rsl-overlays (cons overlay rsl-overlays))
	(rsl-make-overlays1 (cdr poss)))))

(defun rsl-show-overlays ()
  (let ((ovs rsl-overlays))
    (while ovs
      (overlay-put (car ovs) 'face 'trailing-whitespace)
      (setq ovs (cdr ovs)))))

(defun rsl-cancel-test-coverage ()
  (interactive)
  (rsl-clear-overlays))

(defun rsl-has-test-coverage-overlays ()
  (interactive)
  (consp rsl-overlays))

; not used
(defun rsl-offer-to-delete (file)
  (interactive)
  (if (y-or-n-p (format "Coverage file %s out of date.  Delete? " file))
      (delete-file file)))

(defun rsl-same-contents (f1 f2)
  "Return t if files have same contents."
  (let ((res 
	 (apply 'call-process compare-command nil nil nil
 		(list f1 f2))))
    (and (numberp res) (eq res 0))))
  

(defun rsl-save-coverage (file num)
  "Rename file to next non-existent name by concatenating
the necessary increment of num.  
But if an existing file is found with the same contents, replace it."
  (interactive)
  (if (file-exists-p file)
      (let* ((next num)
	     (next-file (concat file (int-to-string next))))
	(while (file-exists-p next-file)
	  (if (rsl-same-contents file next-file)
	      (delete-file next-file)
	    (progn
	      (setq next (1+ next))
	      (setq next-file (concat file (int-to-string next))))))
	(rename-file file next-file))))

(defun rsl-has-old ()
  (interactive)
  (let* ((file (buffer-file-name))
	 (base (file-name-nondirectory (concat file ".el")))
	 (files (rsl-keep-old file (file-name-all-completions base ""))))
    (consp files)))

(defun rsl-delete-coverage ()
  (interactive)
  (let* ((file (buffer-file-name))
	 (base (file-name-nondirectory (concat file ".el")))
	 (files (file-name-all-completions base "")))
    (while files
      (delete-file (car files))
      (setq files (cdr files)))))

  
(defun rsl-delete-old ()
  (interactive)
  (let* ((file (buffer-file-name))
	 (base (file-name-nondirectory (concat file ".el")))
	 (files (rsl-keep-old file (file-name-all-completions base ""))))
    (while files
      (delete-file (car files))
      (setq files (cdr files)))))

(defun rsl-keep-old (file files)
  (if (consp files)
      (let ((next (car files))
	    (rest (rsl-keep-old file (cdr files))))
	(if (up-to-date next file)
	    rest
	  (cons next rest)))
    nil))

(defun rsl-skip-old (file files)
  (if (consp files)
      (let ((next (car files))
	    (rest (rsl-skip-old file (cdr files))))
	(if (not (up-to-date next file))
	    rest
	  (cons next rest)))
    nil))

(defun rsl-skip-same1 (file files)
  "Delete from files any that have the same contents but different name from file."
  (if (consp files)
      (let ((next (car files))
	    (rest (rsl-skip-same1 file (cdr files))))
	(if (and (not (string-equal file next))
		 (rsl-same-contents file next))
	    rest
	  (cons next rest)))
    nil))

(defun rsl-skip-same (file files)
  "Delete from files any that have the same contents but different name from file, if that exists."
  (if (file-exists-p file)
      (rsl-skip-same1 file files)
    files))

(defun rsl-show-test-coverage ()
  "Merges results from coverage files for current buffer.

Current buffer should be an rsl file.

At least one coverage file should exist."
  (interactive)
  (let* ((file (buffer-file-name))
	 (base (file-name-nondirectory (concat file ".el")))
	 (files (rsl-skip-old file (file-name-all-completions base ""))))
    (setq files (rsl-skip-same base files))
    (if (consp files)
	(let* ((first-res (car files)))
	  (load (expand-file-name first-res) nil t t)
	  (setq files (cdr files))
	  (while (consp files)
	    (rsl-merge-test-coverage (car files))
	    (setq files (cdr files)))
	  (rsl-make-overlays)
	  (if (null rsl-overlays)
	      (message "Coverage is complete")
	    (message "Unexecuted expressions displayed in red")))
      (error "No up-to-date coverage files %s* found" base))))

(defun rsl-has-current-test-coverage ()
  (interactive)
  (let* ((file (buffer-file-name))
	 (base (file-name-nondirectory (concat file ".el")))
	 (files (rsl-skip-old file (file-name-all-completions base ""))))
    (consp files)))

(defun rsl-has-test-coverage ()
  (interactive)
  (let* ((file (buffer-file-name))
	 (base (file-name-nondirectory (concat file ".el")))
	 (files (file-name-all-completions base "")))
    (consp files)))

(defun rsl-intersect-region (r1 r2)
  "Intersection of two regions, each a list of beginning and end."
  (let* ((beg1 (car r1))
	 (beg2 (car r2))
	 (beg (max beg1 beg2))
	 (fin1 (car (cdr r1)))
	 (fin2 (car (cdr r2)))
	 (fin (min fin1 fin2)))
    (if (< beg fin)
	(list beg fin)
      nil)))

(defun rsl-intersect-coverage (pos poss)
  "Calculate intersections of region and list of regions"
  (if (consp poss)
      (let ((pos1 (rsl-intersect-region pos (car poss)))
	    (rest (rsl-intersect-coverage pos (cdr poss)))) 
	(if (consp pos1)
	    (cons pos1 rest)
	  rest))
    nil))

(defun rsl-merge-coverages (poss0 poss1)
  "Calculate intersections of regions of two lists"
  (if (consp poss0)
      (let ((poss (rsl-intersect-coverage (car poss0) poss1))
	    (rest (rsl-merge-coverages (cdr poss0) poss1)))
	(append poss rest))
    nil))

(defun rsl-merge-test-coverage (file)
  "File has the form (setq rsl-overlay-poss list).

Merge list with existing contents of rsl-overlay-poss.

Will give nil if rsl-overlay-poss is nil, so do not use for initial load of coverage file."
  (interactive)
  (if (file-exists-p file)
      (let ((poss rsl-overlay-poss))
	(load (expand-file-name file) nil t t)
	(setq rsl-overlay-poss (rsl-merge-coverages poss rsl-overlay-poss)))))
    


;; -------------------------------------------------------
;; support for model checking
;; -------------------------------------------------------

(defun rsltc-sal-make ()
  (interactive)
  (if (eq system-type 'windows-nt)
      (shell-command (concat "copy " rsltc-sal-directory "\\*_prelude ."))
    (shell-command (concat "cp " rsltc-sal-directory "/*_prelude .")))
  (shell-command (concat "make -f " rsltc-sal-directory "/sal_make"))
  ;; should check if this works; perhaps can rely on minibuffer
  (if (eq system-type 'windows-nt)
      (setq shell-file-name explicit-shell-file-name))
  )

(defun rsltc-sal-wfc ()
  (interactive)
  (rsltc-sal-make)
  (compile
   (concat rsltc-sal-directory-unix "/sal_wfc_check "
	   (file-name-sans-extension
	    (file-name-nondirectory (buffer-file-name))))))

(defun rsltc-sal-deadlock-checker (transition-system)
  (interactive "sTransition system identifer: ")
  (rsltc-sal-make)
  (compile
   (concat "sal-deadlock-checker "
	   (file-name-sans-extension
	    (file-name-nondirectory (buffer-file-name)))
	   " "
	   transition-system)
   ))

(defun rsltc-sal-deadlock-checker-cc (transition-system)
  (interactive "sTransition system identifer: ")
  (rsltc-sal-make)
  (compile
   (concat "sal-deadlock-checker "
	   (file-name-sans-extension
	    (file-name-nondirectory (buffer-file-name)))
	   "_cc "
	   transition-system)
   ))

(defun rsltc-sal-deadlock-checker-cc-simple (transition-system)
  (interactive "sTransition system identifer: ")
  (rsltc-sal-make)
  (compile
   (concat "sal-deadlock-checker "
	   (file-name-sans-extension
	    (file-name-nondirectory (buffer-file-name)))
	   "_cc_simple "
	   transition-system)
   ))

(defun rsltc-sal-smc (assertion)
  (interactive "sAssertion identifer (default all assertions): ")
  (rsltc-sal-make)
  (compile
   (concat "sal-smc "
	   (file-name-sans-extension
	    (file-name-nondirectory (buffer-file-name)))
	   " "
	   assertion)
   ))

(defun rsltc-sal-smc-cc (assertion)
  (interactive "sAssertion identifer (default all assertions): ")
  (rsltc-sal-make)
  (compile
   (concat "sal-smc "
	   (file-name-sans-extension
	    (file-name-nondirectory (buffer-file-name)))
	   "_cc "
	   assertion)
   ))

(defun rsltc-sal-smc-cc-simple (assertion)
  (interactive "sAssertion identifer (default all assertions): ")
  (rsltc-sal-make)
  (compile
   (concat "sal-smc "
	   (file-name-sans-extension
	    (file-name-nondirectory (buffer-file-name)))
	   "_cc_simple "
	   assertion)
   ))

(defun rsl-has-new-sal-file ()
  (let* ((rslfile (buffer-file-name))
	 (salfile (concat (file-name-sans-extension rslfile) ".sal")))
	 ; considered new enough if rsl file is not newer
	 ; to avoid problems when times not distinguishable
    (not (file-newer-than-file-p rslfile salfile))))

(defun rsl-has-new-int-ops-file ()
  (let ((int-ops-m4 "Int__OPS.m4")
	(int-ops-sal "Int__OPS.sal"))
    (and
     (and (file-exists-p int-ops-m4) (file-exists-p int-ops-sal))
     (not (file-newer-than-file-p int-ops-m4 int-ops-sal)))))

(defun rsl-can-run-sal ()
  (and
   (rsl-has-new-sal-file) (rsl-has-new-int-ops-file)))
    
(provide 'rsltc)



