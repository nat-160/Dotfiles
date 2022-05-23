(setq inhibit-splash-screen t	    ;; Disable welcome screen
      ring-bell-function 'ignore  ;; Disable error bell
      display-line-numbers-type t ;; Static line number spacing
)
(setq-default indent-tabs-mode nil) ;; Disable tabs globally

;; Delete spaces at line ends when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable the startup message
(defun nat/startup-message () (message ""))
(add-hook 'emacs-startup-hook 'nat/startup-message)

(tooltip-mode -1)    ;; Disable GUI hover
(tool-bar-mode -1)   ;; Disable the bar with icons
(menu-bar-mode -1)   ;; Disable the bar with text
(scroll-bar-mode -1) ;; Disable scroll bars
(set-fringe-mode 16) ;; Space between window and frame

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
;; Initialize sources for the first time
(unless package-archive-contents
  (package-refresh-contents))

;; Install if not detected
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
;; Automatically install packages when loading
(setq use-package-always-ensure t)

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 105)
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height 105)
(set-face-attribute 'variable-pitch nil :font "Source Sans Pro" :height 130 :weight 'regular)

(use-package mixed-pitch
  :custom
  (mixed-pitch-variable-pitch-cursor nil "Use default cursor")
  :hook
  (org-mode . mixed-pitch-mode))

(require 'ox) ;; For exporting
(require 'org-tempo) ;; For templates

(defun nat/org-mode-setup()
  (org-indent-mode)       ;; Visually indent
  (visual-line-mode 1)    ;; Cursor follows word wrap
)
(use-package org
  :custom
  (org-ellipsis " ▾" "Minimized header indicator")
  (org-src-fontify-natively t "Mixed fonts")
  :config
  (nat/org-mode-setup)
  :hook
  (org-mode . nat/org-mode-setup)
)

;; Add list of babel languages
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))
;; Add conf-unix-mode manually
(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Export function
(defun nat/tangle-config ()
  (when
    (string-equal
      (buffer-file-name)
      (expand-file-name "~/Dotfiles/emacs/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
;; Execute when saving Org
(add-hook 'org-mode-hook
  (lambda () (add-hook 'after-save-hook 'nat/tangle-config)))

(use-package modus-themes
  :init ;; Customize before load
  (modus-themes-load-themes)
  :config
  (load-theme 'modus-vivendi t))
