;; MELPA
;; Try use-package to initialize packages at first startup
(package-initialize)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Uncomment if you get elpa signature failed errors
;;(setq package-check-signature nil)
;; Uncomment for initial setup
;;(package-refresh-contents)

(dolist (pkg '(use-package))
  (unless (package-installed-p pkg)
    (message "Trying to install %s" pkg)
    (package-install pkg))
  (require pkg))

;; set the theme to tango dark
(load-theme 'tango-dark)

;; Add the custom directory to pick the custom emacs package configurations
(add-to-list 'load-path "~/.emacs.d/custom")

;;----------------------------------------------------------------------
;; Personal details
;;----------------------------------------------------------------------
(defvar email_address "ammar_husain@apple.com")
(defvar name "Ammar Husain")

;;----------------------------------------------------------------------
;; C/C++ programming
;;-------------------------------------------------------------------------
(require 'cc-mode)

(defun dg-insert-file-comment ()
  "Adds a comment block for a file"
  (interactive)
  (insert "// Copyright (C) 2016 Apple Inc. All Rights Reserved.\n"
	  "/// \\brief  \n")
  )

;; insert a comment
(defun dg-insert-hack-comment ()
  (interactive)
  (insert "/// \\hack(ammar):  ")
  )

;; insert a comment
(defun dg-insert-todo-comment ()
  (interactive)
  (insert "/// \\todo(ammar): ")
  )

;; treat .h files as C++
(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c$" . c++-mode) auto-mode-alist))

;; shortcuts to add auto-generated C/C++ code from functions above
(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map "\C-z\C-f" 'dg-insert-file-comment)
	    (define-key c-mode-base-map "\C-z\C-h" 'dg-insert-hack-comment)
	    (define-key c-mode-base-map "\C-z\C-t" 'dg-insert-todo-comment)
	    )
	  )

;;--------------------------------------------------------------------------
;; dirtree (equivalent of NERDTree in Vim)
;;--------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/custom/emacs-dirtree-master")
(require 'dirtree)

;;---------------------------------------------------------------------------
;; load Doxymacs for doxygen autogenerated blocks
;;---------------------------------------------------------------------------
(require 'doxymacs)
(add-hook 'c-mode-common-hook'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(setq doxymacs-doxygen-style "JavaDoc")

;;----------------------------------------------------------------------
;; Iedit: modify several instances of a variable name simultaneously
;;----------------------------------------------------------------------
(use-package iedit
  :ensure t
  :config
  (define-key iedit-mode-occurrence-keymap (kbd "C-i f") 'iedit-restrict-function)
  (define-key iedit-mode-occurrence-keymap (kbd "C-i s") 'iedit-goto-first-occurrence)
  (define-key iedit-mode-occurrence-keymap (kbd "C-i e") 'iedit-goto-last-occurrence)
  (define-key iedit-mode-occurrence-keymap (kbd "C-i n") 'iedit-number-occurrences)
  )

;;----------------------------------------------------------------------
;; Common-Lisp formatting package
;;----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/custom/cl-lib/")
(require 'cl-lib)

;;----------------------------------------------------------------------
;; Clang formatting
;;----------------------------------------------------------------------
(require 'clang-format)
(global-set-key [C-tab] 'clang-format-region)


;;----------------------------------------------------------------------
;; CMake
;;----------------------------------------------------------------------
					; Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(autoload 'cmake-mode "~/.emacs.d/custom/cmake-mode.el" t)

;;----------------------------------------------------------------------
;; CUDA
;;----------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;;----------------------------------------------------------------------
;; Delete trailing whitespace
;;----------------------------------------------------------------------
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;----------------------------------------------------------------------
;; Fireplace
;;----------------------------------------------------------------------
(load "~/.emacs.d/custom/fireplace/fireplace")

;;----------------------------------------------------------------------
;; Windowing keys
;;----------------------------------------------------------------------
(global-set-key (kbd "<M-left>") 'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)

(global-set-key [(C-M-left)] 'shrink-window-horizontally)
(global-set-key [(C-M-right)] 'enlarge-window-horizontally)
(global-set-key [(C-M-up)] 'enlarge-window)
(global-set-key [(C-M-down)] 'shrink-window)


(setq compilation-read-command nil)
(global-set-key [f9] 'compile)

;;----------------------------------------------------------------------
;; Comment region
;;----------------------------------------------------------------------
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

;; Stop startup message
(setq inhibit-startup-message t)

;; Display row and column
(setq line-number-mode t)
(setq column-number-mode t)

;; Get the mouse to work even when I'm using SSH.
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; feature for revert split pane config. Call winner-undo 【Ctrl+c ←】 and winner-redo 【Ctrl+c →】
(winner-mode 1)

(set-keyboard-coding-system nil)
(setq x-alt-keysym 'meta)

;; File Name
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key (kbd "\C-f") 'show-file-name)


;; Ivy & Counsel
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  )
(use-package counsel
  :ensure t
  :config
  ;; (setq ivy-use-virtual-buffers t)
  ;; (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c C-f") 'counsel-git)
  (global-set-key (kbd "C-c C-g") 'counsel-git-grep)
  (global-set-key (kbd "C-c C-k") 'counsel-ag)
  (global-set-key (kbd "C-c C-l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  )

;; Company
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-require-match nil)
  (global-company-mode t))

(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

; Flycheck does inline highlighting for syntax/compile errors
(use-package flycheck-irony
  :ensure t
  :config
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  )

;; Disabled because it interefers with iedit buffers
;; (use-package irony-eldoc
;;   :ensure t
;;   :config
;;   (add-hook 'irony-mode-hook 'irony-eldoc))

(defun counsel-irony-mode-hook ()
  (define-key irony-mode-map
    [remap completion-at-point] 'counsel-irony)
  (define-key irony-mode-map
    [remap complete-symbol] 'counsel-irony))
(add-hook 'irony-mode-hook 'counsel-irony-mode-hook)

;;----------------------------------------------------------------------
;; GUD (GDB) Debugging
;;----------------------------------------------------------------------
;;Make up/down behave as in terminal
(add-hook 'gud-mode-hook
          '(lambda ()
             (local-set-key [home] ; move to beginning of line, after prompt
                            'comint-bol)
             (local-set-key [up] ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
             (local-set-key [down] ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))
             )
	  ;; By default set it to the multi window arrangement
          (setq gdb-many-windows t))

;; Remap keybindings to Ctrl-Q for easy usability
(global-set-key (kbd "C-q") nil)
(global-set-key (kbd "C-q C-s") 'gud-step)
(global-set-key (kbd "C-q C-b") 'gud-break)
(global-set-key (kbd "C-q C-d") 'gud-remove)
(global-set-key (kbd "C-q C-n") 'gud-next)
(global-set-key (kbd "C-q C-p") 'gud-print)
(global-set-key (kbd "C-q C-j") 'gud-jump)
(global-set-key (kbd "C-q C-c") 'gud-cont)
(global-set-key (kbd "C-q C-u") 'gud-until)
(global-set-key (kbd "C-q C-w") 'gud-watch)
(global-set-key (kbd "C-q <") 'gud-up)
(global-set-key (kbd "C-q >") 'gud-down)
(global-set-key (kbd "C-q C-g") 'gdb)


;;----------------------------------------------------------------------
;; END OF HUMAN FILE
;;----------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ivy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
