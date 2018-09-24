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

(require 'cc-mode)


;;----------------------------------------------------------------------
;; Doxygen autogen functions
;;-------------------------------------------------------------------------
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

;; add useful behaviour to c-mode
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
;; load Doxymacs
;;---------------------------------------------------------------------------
(require 'doxymacs)
(add-hook 'c-mode-common-hook'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)


;; MELPA
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

; Fix iedit bug
; modify several instances of a variable name simultaneously
(define-key global-map (kbd "C-c ;") 'iedit-mode)


(add-to-list 'load-path "~/.emacs.d/custom/cl-lib/")
(require 'cl-lib)


; Clang formatting
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

;; Fireplace
(load "~/.emacs.d/custom/fireplace/fireplace")

;; Windowing keys
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

;; feature for revert split pane config. Call winner-undo 【Ctrl+c ←】 and winner-redo 【Ctrl+c →】
(winner-mode 1)

(set-keyboard-coding-system nil)
(setq x-alt-keysym 'meta)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compile-command "cmake -DCMAKE_BUILD_TYPE=Debug ..; make -j ")
 '(cmake-ide-build-dir "~/src/mBot/ros/build")
 '(package-selected-packages
   (quote
    (flycheck-rtags flycheck-package flycheck-irony company-irony-c-headers company-irony irony cmake-ide rtags ivy iedit google-c-style ggtags flymake-google-cpplint flymake-cursor auto-complete-c-headers))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key (kbd "\C-f") nil)
(global-set-key (kbd "\C-f1") 'show-file-name)

 (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers t)
 (global-set-key "\C-s" 'swiper)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "<f6>") 'ivy-resume)
 (global-set-key (kbd "M-x") 'counsel-M-x)
 (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)




;; Rtags, cmake-ide
(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(require 'helm-rtags)
(setq rtags-use-helm t)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))

(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(setq company-idle-delay 0)
(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(require 'flycheck-rtags)

(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(cmake-ide-setup)

;; http://syamajala.github.io/c-ide.html
;; http://diobla.info/doc/rtags#sec-4-2
;; http://nilsdeppe.com/posts/emacs-c++-ide
