;; Begin: rsl-mode.el
;; RSL mode for [x]emacs 20.

(defvar rsl-mode-syntax-table nil
  "Syntax table in use in rsl-mode buffers.")

(if rsl-mode-syntax-table
    ()
  (setq rsl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?! "_"    rsl-mode-syntax-table)
  (modify-syntax-entry ?\\ "_"   rsl-mode-syntax-table)
  (modify-syntax-entry ?/ "_ 14" rsl-mode-syntax-table)  
  (modify-syntax-entry ?* ". 23" rsl-mode-syntax-table)
  (modify-syntax-entry ?| "."    rsl-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    rsl-mode-syntax-table)
  (modify-syntax-entry ?~ "_"    rsl-mode-syntax-table)
  (modify-syntax-entry ?` "w"    rsl-mode-syntax-table)
  (modify-syntax-entry ?\' "w"   rsl-mode-syntax-table)
  ;; a single hyphen is a symbol, but a double hyphen starts a line comment
  (modify-syntax-entry ?- "_ 12b" rsl-mode-syntax-table)
  ;; \f and \n end a line comment
  (modify-syntax-entry ?\f  "> b" rsl-mode-syntax-table)
  (modify-syntax-entry ?\n  "> b" rsl-mode-syntax-table)
  ;; define parentheses to match
  (modify-syntax-entry ?\( "()"  rsl-mode-syntax-table)
  (modify-syntax-entry ?\) ")("  rsl-mode-syntax-table))


;;;###autoload
(defun rsl-mode ()
  "Major mode for editing RSL code."
  (interactive)
  (kill-all-local-variables)
  (use-local-map rsl-mode-map)
  (setq major-mode 'rsl-mode)
  (setq mode-name "RSL")
;  (setq local-abbrev-table rsl-mode-abbrev-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(rsl-font-lock-keywords nil nil))
  (set-syntax-table rsl-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (local-set-key "\^I" 'indent-relative)
;  (setq comment-indent-function 'rsl-indent-comment)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'block-comment-start)
  (setq block-comment-start "-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'block-comment-end)
  (setq block-comment-end "")
  (make-local-variable 'comment-start-skip) ;; used by autofill
;  (setq comment-start-skip "/\\*+ *\\|--+ *") ;; gives -- in block comments
  (setq comment-start-skip "--+ *")
;  (make-local-variable 'parse-sexp-ignore-comments)
;  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (run-hooks 'text-mode-hook))


(defvar rsl-font-lock-keywords 
  '(("\\<\\(Bool\\|Char\\|Int\\|Nat\\|Real\\|Text\\|Unit\\|a\\(bs\\|l\\(l\\|ways\\)\\|ny\\|s\\|xiom\\)\\|c\\(a\\(rd\\|se\\)\\|ha\\(nnel\\|os\\)\\|lass\\)\\|d\\(evt_relation\\|om?\\)\\|e\\(l\\(ems\\|s\\(e\\|if\\)\\)\\|nd\\|x\\(ists\\|tend\\)\\)\\|f\\(alse\\|or\\)\\|h\\(d\\|ide\\)\\|i\\([fns]\\|n\\(ds\\|itialise\\|t\\(\\|er\\)\\)\\|sin\\)\\|l\\(e[nt]\\|ocal\\|tl_assertion\\)\\|o\\(bject\\|f\\|ut\\)\\|p\\(ost\\|re\\)\\|r\\(ea[dl]\\|ng\\)\\|s\\(cheme\\|kip\\|top\\|wap\\)\\|t\\(he\\(n\\|ory\\)\\|est_case\\|l\\|r\\(ansition_system\\|ue\\)\\|ype\\)\\|u\\(n\\(ion\\|til\\)\\|se\\)\\|va\\(lue\\|riable\\)\\|w\\(hile\\|ith\\|rite\\)\\|~isin\\)\\>" .  font-lock-keyword-face)
    ("-\\(inf\\(list\\|set\\)\\|list\\|set\\)\\>" .  font-lock-keyword-face)
    ("\<:.*:\>" 0 font-lock-comment-face)
    ("\{!.*!\}" 0 font-lock-comment-face)
    ("/\\*" ".*\\*/" 0 font-lock-comment-face)  
    ("--.*$" 0 font-lock-comment-face)
    ("^[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*(.*)[ \t]*\\(is\\>\\|as\\>\\)" 1 font-lock-function-name-face)
    ("^[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*:[^:]" 1 font-lock-function-name-face)
    ("\\<[A-Z][A-Za-z0-9_]*" 0 font-lock-type-face)
    ("\"" ".*\"" 0 font-lock-string-face)
    ("\'.\'" 0 font-lock-string-face)
    )
  "Expressions to highlight in RSL mode.")


(setq auto-mode-alist (cons '("\\.rsl\\'" . rsl-mode) auto-mode-alist))

(require 'sml-site)

(require 'rsltc)

(require 'easymenu)

(defvar rsl-mode-map (make-sparse-keymap)
  "Keymap used in RSL mode.")

(easy-menu-define
 rsl-mode-menu
 rsl-mode-map
 "Menu for rsl commands."
 '("RSL"
   ["Type check" rsltc-only t]
   ["Pretty-print" rslpp-dflt t]
   ["Generate confidence conditions (this file)" rsltc-c t]
   ["Generate confidence conditions (all files)" rsltc-cc t]
   ["Draw module dependency graph" rslgg t]
   ["Show module dependencies" rsltc-d t]
   ["Comment Region" rsl-comment-region mark-active]
   ["Uncomment Region" rsl-uncomment-region mark-active]
   ["Translate to C++" rsltc-cpp t]
   ["Translate to Visual C++" rsltc-cppv t]
   ["Translate to PVS" rsltc-pvs t]
   ("SML"
    ["Translate to SML" rsltc-m t]
    ["Translate to SML and run" rsltc-m-and-run t]
    ["Run SML file (Ctrl-c Ctrl-c to interrupt)" rsl-sml-load (rsl-has-new-sml-file)]
    ["End SML run and save results" rsl-sml-results (rsl-have-sml-buff)])
   ("SAL"
    ["Translate to SAL" rsltc-sal t]
    ["Run SAL well-formed checker" rsltc-sal-wfc (rsl-has-new-sal-file)]
    ("Run SAL deadlock checker" :active (rsl-can-run-sal)
     ["Base" rsltc-sal-deadlock-checker (rsl-can-run-sal)]
     ["CC" rsltc-sal-deadlock-checker-cc (rsl-can-run-sal)]
     ["Simple CC" rsltc-sal-deadlock-checker-cc-simple (rsl-can-run-sal)])
    ("Run SAL model checker" :active (rsl-can-run-sal)
     ["Base" rsltc-sal-smc (rsl-can-run-sal)]
     ["CC" rsltc-sal-smc-cc (rsl-can-run-sal)]
     ["Simple CC" rsltc-sal-smc-cc-simple (rsl-can-run-sal)]))
   ["Translate to CSP" rsltc-csp t]
   ("Mutation testing"
    ["Make mutation" rsl-mutate mark-active]
    ["Compare with mutant" rsl-compare-with-mutant (or (has-mutant-dirs)
						       (is-mutant-dir))]
    ["Delete mutant directories" rsl-delete-mutant-dirs (has-mutant-dirs)])
   ("Test coverage"
    ["Show test coverage" rsl-show-test-coverage (rsl-has-current-test-coverage)]
    ["Cancel test coverage" rsl-cancel-test-coverage (rsl-has-test-coverage-overlays)]
    ["Delete old coverage results" rsl-delete-old (rsl-has-old)]
    ["Delete all coverage results" rsl-delete-coverage (rsl-has-test-coverage)])
    
   ))

(define-key rsl-mode-map "\C-c;" 'rsl-comment-region)
(define-key rsl-mode-map "\C-c:" 'rsl-uncomment-region)

;; (define-key rsl-mode-map [menu-bar rsl]
;;   (cons "RSL" (make-sparse-keymap "RSL")))

;; (define-key rsl-mode-map [menu-bar rsl rsl-delete-coverage]
;;   '("Delete all coverage results" . rsl-delete-coverage))
;; (put 'rsl-delete-coverage 'menu-enable `(rsl-has-test-coverage))
;; (define-key rsl-mode-map [menu-bar rsl rsl-delete-old]
;;   '("Delete old coverage results" . rsl-delete-old))
;; (put 'rsl-delete-old 'menu-enable `(rsl-has-old))
;; (define-key rsl-mode-map [menu-bar rsl rsl-cancel-test-coverage]
;;   '("Cancel test coverage" . rsl-cancel-test-coverage))
;; (put 'rsl-cancel-test-coverage 'menu-enable `(rsl-has-test-coverage-overlays))
;; (define-key rsl-mode-map [menu-bar rsl rsl-show-test-coverage]
;;   '("Show test coverage" . rsl-show-test-coverage))
;; (put 'rsl-show-test-coverage 'menu-enable `(rsl-has-current-test-coverage))

;; (define-key rsl-mode-map [menu-bar rsl rsl-mutate]
;;   '("Make mutation" . rsl-mutate))
;; (put 'rsl-mutate 'menu-enable 'mark-active)
;; (define-key rsl-mode-map [menu-bar rsl rsl-delete-mutant-dirs]
;;   '("Delete mutant directories" . rsl-delete-mutant-dirs))
;; (put 'rsl-delete-mutant-dirs 'menu-enable `(has-mutant-dirs))
;; (define-key rsl-mode-map [menu-bar rsl rsl-compare-with-mutant]
;;   '("Compare with mutant" . rsl-compare-with-mutant))
;; (put 'rsl-compare-with-mutant 'menu-enable `(or (has-mutant-dirs)
;; 						(is-mutant-dir)))

;; (define-key rsl-mode-map [menu-bar rsl rsl-uncomment-region]
;;   '("Uncomment Region" . rsl-uncomment-region))
;; (put 'rsl-uncomment-region 'menu-enable 'mark-active)
;; (define-key rsl-mode-map [menu-bar rsl rsl-comment-region]
;;   '("Comment Region" . rsl-comment-region))
;; (put 'rsl-comment-region 'menu-enable 'mark-active)

;; (define-key rsl-mode-map [menu-bar rsl rsltc-sal-smc]
;;   '("Run SAL model checker" . rsltc-sal-smc))
;; (put 'rsltc-sal-smc 'menu-enable `(rsl-has-new-sal-file))
;; (define-key rsl-mode-map [menu-bar rsl rsltc-sal-deadlock-checker]
;;   '("Run SAL deadlock checker" . rsltc-sal-deadlock-checker))
;; (put 'rsltc-sal-deadlock-checker 'menu-enable `(rsl-has-new-sal-file))
;; (define-key rsl-mode-map [menu-bar rsl rsltc-sal-wfc]
;;   '("Run SAL well-formed checker" . rsltc-sal-wfc))
;; (put 'rsltc-sal-wfc 'menu-enable `(rsl-has-new-sal-file))
;; (define-key rsl-mode-map [menu-bar rsl rsltc-sal]
;;   '("Translate to SAL" . rsltc-sal))

;; (define-key rsl-mode-map [menu-bar rsl rsl-sml-results]
;;       '("Save results from SML run" . rsl-sml-results))
;; (put 'rsl-sml-results 'menu-enable '(rsl-have-sml-buff))

;; (define-key rsl-mode-map [menu-bar rsl rsl-sml-load]
;;       '("Run SML file (Ctrl-d to end; Ctrl-c Ctrl-c to interrupt)" . rsl-sml-load))
;; (put 'rsl-sml-load 'menu-enable '(rsl-has-new-sml-file))

;; (define-key rsl-mode-map [menu-bar rsl rsltc-m-and-run]
;;   '("Translate to SML and run" . rsltc-m-and-run))
;; (define-key rsl-mode-map [menu-bar rsl rsltc-m]
;;   '("Translate to SML" . rsltc-m))
;; (define-key rsl-mode-map [menu-bar rsl rsltc-pvs]
;;   '("Translate to PVS" . rsltc-pvs))
;; ;(define-key rsl-mode-map [menu-bar rsl rsltc-sqlv]
;; ;  '("Translate to Visual C++ with SQL" . rsltc-sql))
;; (define-key rsl-mode-map [menu-bar rsl rsltc-cppv]
;;   '("Translate to Visual C++" . rsltc-cppv))
;; ;(define-key rsl-mode-map [menu-bar rsl rsltc-sql]
;; ;  '("Translate to C++ with SQL" . rsltc-sql))
;; (define-key rsl-mode-map [menu-bar rsl rsltc-cpp]
;;   '("Translate to C++" . rsltc-cpp))
;; (define-key rsl-mode-map [menu-bar rsl rsltc-d]
;;   '("Show module dependencies" . rsltc-d))
;; (define-key rsl-mode-map [menu-bar rsl rslgg]
;;   '("Draw module dependency graph" . rslgg))
;; (define-key rsl-mode-map [menu-bar rsl rsltc-cc]
;;   '("Generate confidence conditions (all files)" . rsltc-cc))
;; ;(define-key rsl-mode-map [menu-bar rsl rsltc-pc]
;; ;  '("Generate confidence conditions for proof (this file)" . rsltc-pc))
;; (define-key rsl-mode-map [menu-bar rsl rsltc-c]
;;   '("Generate confidence conditions (this file)" . rsltc-c))
;; (define-key rsl-mode-map [menu-bar rsl rslpp-dflt]
;;   '("Pretty-print" . rslpp-dflt))
;; (define-key rsl-mode-map [menu-bar rsl rsltc-only]
;;   '("Type check" . rsltc-only))


(defun rsl-comment-region (beg end)
  "Comments region using line comments."
  (interactive "*r")
  (let ((save-comment-start comment-start)
	(save-comment-end comment-end))
    (unwind-protect
	(progn
	  (setq comment-start block-comment-start comment-end block-comment-end)
	  (comment-region beg end))
      (setq comment-start save-comment-start comment-end save-comment-end))))

(defun rsl-uncomment-region (beg end)
  "Uncomments region using line comments."
  (interactive "*r")
  (let ((save-comment-start comment-start)
	(save-comment-end comment-end))
    (unwind-protect
	(progn
	  (setq comment-start block-comment-start comment-end block-comment-end)
	  (uncomment-region beg end))
      (setq comment-start save-comment-start comment-end save-comment-end))))


(define-key rsl-mode-map "\C-c\C-l" 'rsl-sml-load)


(provide 'rsl-mode)
;; End: rsl-mode.el
