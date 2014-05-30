;;----------------------------------------------------------------------
;; C/C++ programming
;;-------------------------------------------------------------------------
;; insert a nice doxygen function documentation block

(defvar email_address "ahusain@nrec.ri.cmu.edu")
(defvar name "Ammar Husain")

(defun dg-insert-function-header ()
  (interactive)
  (insert "/*! -------------------------------------------------------------- \n"
          " * \@brief \n"
          " *\n"
          " * \@author: " name " (" email_address ")\n"
          " * \@date " (format-time-string "%m/%d/%Y") "\n"
          " * ---------------------------------------------------------------- \n"
	  " */\n")
  (search-backward "brief")
  (end-of-line))

;; insert a nice doxygen class documentation block
(defun dg-insert-class-header (class-name)
  "Prompts for the name of a new class, then writes suitable Doxygen markup
for the class documentation and inserts lines for the ctor/dtor pair."
  (interactive "sClass Name: ")
  (insert "/** -------------------------------------------------------------- \n"
          " * \@class  " class-name "\n"
          " * \n"
          " * \@author " name " <" email_address ">\n"
          " * \@date   " (format-time-string "%m/%d/%Y") "\n"
          " * \n"
          " * \@brief  \n"
          " * \n"
          " * ------------------------------------------------------------ */\n"
          "class " class-name "\n"
          "{\n"
          "public:\n"
          "    " class-name "();\n"
          "    virtual ~" class-name "();\n\n"
          "protected:\n"
          "};\n")
  (search-backward "brief")
  (end-of-line))

;; pick out what kind of file we're dealing with and insert an appropriate
;; header block
(defun dg-insert-file-header ()
  "Checks the buffer name to get the filename; If extension is {h,hpp}, inserts
include guards and other header file information.  If extension is {c,cpp,cc},
it only includes basic header information"
  (interactive)
  (let* ((filename (buffer-name))
         (basename (file-name-sans-extension filename))
         (extension (file-name-extension filename)))
    (goto-char (point-min))
    (cond
     ((or (string= extension "h")
          (string= extension "hpp"))
      (dg-insert-header-file-header basename extension))
     ((or (string= extension "cpp")
          (string= extension "cc")
          (string= extension "c")
          (string= extension "c++"))
      (dg-insert-cpp-file-header basename)))))

;; insert a nice doxygen file documentation block and preprocessor template
(defun dg-insert-header-file-header (basename extension)
  (interactive)
  (insert "#ifndef _" (upcase basename) "_" (upcase extension) "_\n"
          "#define _" (upcase basename) "_" (upcase extension) "_\n\n"
          "/** -------------------------------------------------------------- \n"
          " * \@file   " (file-name-nondirectory (buffer-file-name)) "\n"
          " * \n"
          " * \@author " name " <" email_address ">\n"
          " * \@date   " (format-time-string "%m/%d/%Y") "\n"
	  " * \n"
	  " * \@brief  \n"
	  " * \n"
	  " * \n"
          " * ------------------------------------------------------------ */\n")
  (goto-char (point-max))
  (move-beginning-of-line nil)
  (insert "\n#endif\n")
  (move-beginning-of-line -1))

;; insert a nice doxygen file documentation block
(defun dg-insert-cpp-file-header (basename)
  (interactive)
  (insert "/*! -------------------------------------------------------------- \n"
          " * \@file   " (file-name-nondirectory (buffer-file-name)) "\n"
          " *\n"
          " * \@author " name " (" email_address ")\n"
          " * \@date   " (format-time-string "%m/%d/%Y") "\n"
          " * ---------------------------------------------------------------- \n"
          " */\n"))


;; insert a nice code separation block
(defun dg-insert-code-separation ()
  (interactive)
  (insert "/// ---------------------------------------------------------------------- \n")
  )

;; insert the author tag with name
(defun dg-insert-author-tag ()
  (interactive)
  (insert "\@author: " name " <" email_address ">")
  )

;; insert an include macro
(defun dg-insert-include ()
  (interactive)
  (insert "#include <> \n")
  )

;; insert a comment
(defun dg-insert-comment ()
  (interactive)
  (insert "// ")
  )


;; insert a comment
(defun dg-insert-hack-comment ()
  (interactive)
  (insert "/// -------------!!!!!!!!!    HACK :(   !!!!!!!!!------------- ///")
  )

;; insert a comment
(defun dg-insert-todo-comment ()
  (interactive)
  (insert "/// !!!!---- TODO: ")
  )


(defun add-qt-customizations ()
  "add some customizations that enable emacs to treat Qt idioms as language builtins"
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected\\|"
                                 "protected slot\\|private\\|private slot\\)\\>"))
  (setq c-C++-access-key (concat "\\<\\(signals\\|public\\|private\\|protected\\|"
                                 "public slots\\|protected slots\\|private slots\\)\\>[ \t]*:"))
  (font-lock-add-keywords 'c++-mode
                          '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
  
  ;; set up a font-lock face for the Q* symbols
  (make-face 'qt-keywords-face)
  (set-face-foreground 'qt-keywords-face "BlueViolet")
  (font-lock-add-keywords 'c++-mode '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode '(("Q_OBJECT" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<Q[A-Z3][A-Za-z0-9]*" . 'qt-keywords-face)))
  
  ;; and treat .pro files like makefiles
  (setq auto-mode-alist (cons '("\\.pro$" . makefile-mode) auto-mode-alist))
  )

(defun vlad-cc-style()
  (c-set-style "linux")
  (c-set-offset 'innamespace '0)
  (c-set-offset 'inextern-lang '0)
  (c-set-offset 'inline-open '0)
  (c-set-offset 'label '*)
  (c-set-offset 'case-label '*)
  (c-set-offset 'access-label '/)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  )

;; no indentation with namespaces
(defconst my-cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc-mode" my-cc-style)

;; treat .h files as C++
(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c$" . c++-mode) auto-mode-alist))

(set-face-attribute 'default nil :height 100)

;; add useful behaviour to c-mode
(add-hook 'c-mode-hook
          (lambda ()
            (c-toggle-auto-hungry-state 1)
            (c-set-style "ellemtel")
            (setq c-basic-offset 4)
            (define-key c-mode-map "\C-m" 'reindent-then-newline-and-indent)
            (define-key c++-mode-map "\C-c\C-v" 'dg-insert-code-separation)
            (define-key c++-mode-map "\C-c\C-c" 'dg-insert-comment)
            (define-key c++-mode-map "\C-c\C-i" 'dg-insert-include)
            (define-key c++-mode-map "\C-c\C-h" 'dg-insert-hack-comment)
            (define-key c++-mode-map "\C-c\C-t" 'dg-insert-todo-comment)
            (define-key c++-mode-map "\C-c\C-a" 'dg-insert-author-tag)
	    )
)

;; add useful behaviour to c++-mode
(add-hook 'c++-mode-hook
          (lambda ()
            ;;(add-qt-customizations)
            (c-toggle-auto-hungry-state 1)
            (c-set-style "ellemtel")
	    (vlad-cc-style)
	    ;;(setq indent-line-function 'insert-tab)
            (define-key c++-mode-map "\C-m" 'reindent-then-newline-and-indent)
            (define-key c++-mode-map "\C-c\C-d" 'dg-insert-file-header)
            (define-key c++-mode-map "\C-c\C-f" 'dg-insert-function-header)
            (define-key c++-mode-map "\C-c\C-l" 'dg-insert-class-header)
            (define-key c++-mode-map "\C-c\C-v" 'dg-insert-code-separation)
            (define-key c++-mode-map "\C-c\C-c" 'dg-insert-comment)
            (define-key c++-mode-map "\C-c\C-i" 'dg-insert-include)
            (define-key c++-mode-map "\C-c\C-h" 'dg-insert-hack-comment)
            (define-key c++-mode-map "\C-c\C-t" 'dg-insert-todo-comment)
            (define-key c++-mode-map "\C-c\C-a" 'dg-insert-author-tag)
	    )
)


;;--------------------------------------------------------------------------
;; dirtree (equivalent of NERDTree in Vim)
;;--------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/emacs-dirtree-master")
(require 'dirtree)

;;--------------------------------------------------------------------------
;; python programming
;;--------------------------------------------------------------------------
;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (autoload 'python-mode "python-mode" "Python Mode" t)
;; (setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                    interpreter-mode-alist))

;; restore the original save function for python files
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\C-x\C-s" 'save-buffer)))


;;------------------------------------------------------------------------------
;; matlab programming
;;------------------------------------------------------------------------------
(autoload 'matlab-mode "matlab" "Enter Matlab Mode." t)
(autoload 'matlab-shell "matlab" "Interactive Matlab Mode." t)
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
(setq matlab-shell-command "matlab")
(setq matlab-shell-command-switches "-nodesktop -nojvm")
(add-hook 'matlab-mode-hook
          (lambda ()
            (setq matlab-indent-level 4)
            (setq fill-column 80)
            (define-key matlab-mode-map "\M-;" 'comment-dwim)))


;;---------------------------------------------------------------------------
;; emacs checkbook balancer
;;---------------------------------------------------------------------------
(autoload 'balance-mode "balance" "Checkbook Balancing Mode" t)
(setq auto-mode-alist (cons '("\\.bal$" . balance-mode) auto-mode-alist))
(eval-after-load "balance"
  '(define-key balance-mode-map "\C-c\C-v"
     'balance-append-transaction))

;;---------------------------------------------------------------------------
;; function-args and CEDET
;;---------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/function-args")
(require 'function-args)
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-default 'semantic-case-fold t)

;;---------------------------------------------------------------------------
;; load Doxymacs
;;---------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/doxymacs/no-autoconf/")
(load-library "doxymacs")
(add-hook 'c-mode-common-hook'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
