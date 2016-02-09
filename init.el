;; set the theme to tango dark
;(load-theme 'tango-dark)

; Add the custom directory to pick the custom emacs package configurations
(add-to-list 'load-path "~/.emacs.d/custom")


;;----------------------------------------------------------------------
;; C/C++ programming
;;-------------------------------------------------------------------------
;; insert a nice doxygen function documentation block

(defvar email_address "ammar_husain@apple.com")
(defvar name "Ammar Husain")

(add-to-list 'load-path "~/.emacs.d/custom/cl-lib/")
(require 'cl-lib)

(require 'cc-mode)


;;----------------------------------------------------------------------
;; Doxygen autogen functions
;;-------------------------------------------------------------------------
(defun dg-insert-class-comment ()
  "Adds a comment block for a class"
  (interactive)
  (insert "/// \\class  \n"
         ; "/// \\author " name " <" email_address ">\n"
          "///\n"
	  "/// \\brief  \n"
	  "/// \\details \n"
          "///\n"
          "///\n")
   (search-backward "brief")
   (end-of-line)
)

(defun dg-insert-file-comment ()
  "Adds a comment block for a file"
  (interactive)
  (insert "// Copyright (C) 2016 Apple Inc. All Rights Reserved.\n"
	  "/// \\brief  \n")
  )

;; insert a comment
(defun dg-insert-comment ()
  (interactive)
  (insert "// ")
  )

;; insert a comment
(defun dg-insert-doxygen-comment ()
  (interactive)
  (insert "/// ")
  )

;; insert a comment
(defun dg-insert-hack-comment ()
  (interactive)
  (insert "/// \\hack ):    ")
  )

;; insert a comment
(defun dg-insert-todo-comment ()
  (interactive)
  (insert "/// \\todo (ammar_husain): ")
  )

;; treat .h files as C++
(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c$" . c++-mode) auto-mode-alist))


;;--------------------------------------------------------------------------
;; dirtree (equivalent of NERDTree in Vim)
;;--------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/custom/emacs-dirtree-master")
(require 'dirtree)

;;--------------------------------------------------------------------------
;; dirtree (equivalent of NERDTree in Vim)
;;--------------------------------------------------------------------------
; install: sudo pip install cpplint
; start flymake-google-cpplint-load
; let's define a function for flymake initialization
(defun my:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command ;"/usr/local/bin/cpplint"))
     "~/src/autonomy-repo/autonomy/cmake/lint/google_cpplint.py"))
  (flymake-google-cpplint-load)
  )

 (add-hook 'c-mode-hook 'my:flymake-google-init)
 (add-hook 'c++-mode-hook 'my:flymake-google-init)


;;---------------------------------------------------------------------------
;; function-args and CEDET
;;---------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/custom/cedet/function-arg")
(require 'function-args)
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-default 'semantic-case-fold t)

;;---------------------------------------------------------------------------
;; load Doxymacs
;;---------------------------------------------------------------------------
(require 'doxymacs)
(add-hook 'c-mode-common-hook'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

; start package.el with emacs
(require 'package)
; add MELPA to repository list
(add-to-list 'package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
				 ("melpa2" . "http://melpa.org/packages/")))
; initialize package.el
(package-initialize)
; start auto-complete with emacs
(require 'auto-complete)
; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)


; start yasnippet with emacs
;(require 'yasnippet)
;(yas-global-mode 1)


; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
)
; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

; Fix iedit bug
; modify several instances of a variable name simultaneously
(define-key global-map (kbd "C-c ;") 'iedit-mode)

; Clang formatting
(require 'clang-format)
(setq clang-format-executable '"/usr/local/bin/clang-format")
(global-set-key (kbd "C-M-z") 'clang-format-buffer)

; start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; add useful behaviour to c-mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map "\C-z\C-l" 'dg-insert-class-comment)
            (define-key c-mode-base-map "\C-z\C-f" 'dg-insert-file-comment)
            (define-key c-mode-base-map "\C-z\C-c" 'dg-insert-comment)
	    (define-key c-mode-base-map "\C-z\C-d" 'dg-insert-doxygen-comment)
	    (define-key c-mode-base-map "\C-z\C-h" 'dg-insert-hack-comment)
	    (define-key c-mode-base-map "\C-z\C-t" 'dg-insert-todo-comment)
	    )
	  )

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

;; Fireplace
(load "~/.emacs.d/custom/fireplace/fireplace")

;; Windowing keys
(global-set-key [(C-M-left)] 'windmove-left)
(global-set-key [(C-M-right)] 'windmove-right)
(global-set-key [(C-M-up)] 'windmove-up)
(global-set-key [(C-M-down)] 'windmove-down)

;(global-set-key [(C-M-left)] 'shrink-window-horizontally)
;(global-set-key [(C-M-right)] 'enlarge-window-horizontally)
;(global-set-key [(C-M-up)] 'enlarge-window)
;(global-set-key [(C-M-down)] 'shrink-window)

(setq compilation-read-command nil)
(global-set-key [f9] 'compile)

;; Comment region
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

(setq inhibit-startup-message t)

;; Display row and column
(setq line-number-mode t)
(setq column-number-mode t)

;; Get the mouse to work even when I'm using SSH.
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; save/restore opened files and windows config
;(desktop-save-mode 1) ; 0 for off

;; feature for revert split pane config. Call winner-undo 【Ctrl+c ←】 and winner-redo 【Ctrl+c →】
(winner-mode 1)

(set-keyboard-coding-system nil)
(setq x-alt-keysym 'meta)
