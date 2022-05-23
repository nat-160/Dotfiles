(tool-bar-mode -1) ;; Disable the bar with icon
(menu-bar-mode -1) ;; Disable the bar with text
(scroll-bar-mode -1)  ;; Disable scroll bars
(setq inhibit-splash-screen t) ;; Disable the startup screen
(setq ring-bell-function 'ignore) ;; Disable the bell
(defun nat/startup-message () (message ""))
(add-hook 'emacs-startup-hook 'nat/startup-message) ;; Disable the startup message
;; (tooltip-mode -1) ;; Disable GUI hover
(set-fringe-mode 16) ;; Space between global linum and line-num

(global-display-line-numbers-mode t) ;; Enable line numbers
(setq display-line-numbers-type t) ;; Static spacing for line numbers

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 105) ;; Set default font
