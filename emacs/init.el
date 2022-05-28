(setq inhibit-splash-screen t	;; Disable welcome screen
      ring-bell-function 'ignore  ;; Disable error bell
      display-line-numbers-type t ;; Static line number spacing
)
(setq-default indent-tabs-mode nil) ;; Disable tabs globally

;; Delete spaces at line ends when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable the startup message
(defun nat/startup-message () (message ""))
(add-hook 'emacs-startup-hook 'nat/startup-message)

;; Line numbers when coding
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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

(use-package no-littering
  :custom
  (auto-save-file-name-transforms `((".*",(no-littering-expand-var-file-name "auto-save/") t)) "Change location for auto-saved files"))

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 120)
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height 1.0)
(set-face-attribute 'variable-pitch nil :font "Source Sans Pro" :height 1.0)

(defun nat/org-set-faces ()

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Source Sans Pro" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (dolist (face '(line-number
                  line-number-current-line
                  org-block
                  org-block-begin-line
                  org-block-end-line
                  org-code
                  org-checkbox
                  org-document-info-keyword
                  org-formula
                  org-meta-line
                  org-special-keyword
                  org-table
                  org-verbatim))
    (set-face-attribute face nil :inherit 'fixed-pitch))
  )

(require 'ox) ;; For exporting
(require 'org-tempo) ;; For templates

(defun nat/org-mode-setup()
  (org-indent-mode)    ;; Visually indent
  (visual-line-mode 1) ;; Cursor follows word wrap
  (variable-pitch-mode 1)
  (nat/org-set-faces)
  )
(use-package org
  :custom
  (org-ellipsis " ▾" "Minimized header indicator")
  (org-src-fontify-natively t "Mixed fonts")
  (org-src-tab-acts-natively t "Indent code blocks")
  (org-hide-emphasis-markers t "Hide markdown symbols")
  :hook
  (org-mode . nat/org-mode-setup)
  )

;; Add list of babel languages
(org-babel-do-load-languages 'org-babel-load-languages
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

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ps" . "src powershell"))

(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode))

(use-package modus-themes
  :init ;; Customize before load
  (modus-themes-load-themes)
  :config
  (load-theme 'modus-vivendi t))

(use-package powershell
  :custom
  (powershell-indent 2 "Spacing after line")
  )

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c l")
  :hook
  (powershell-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  )

(use-package which-key
  :init
  (which-key-mode)
)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  )

(use-package company
  :after lsp-mode
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay (lambda()(if(company-in-string-or-comment) nil 0.0)))
  :hook
  (lsp-mode . company-mode)
  )
