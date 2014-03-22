;;;; STARTING CONFIGURATION

;; Initial frame size
(when window-system (set-frame-size (selected-frame) 180 55))
(setq inhibit-startup-message t)
(tool-bar-mode -1)

;; Starting directory
(setq default-directory (getenv "HOME"))


;;;; INIT FILE

;; From http://metasandwich.com/2013/01/19/emacs-config-youre-doing-it-wrong/
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)


;;;; MODES

;; Org mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; iswitch: completion in buffer selection
(iswitchb-mode 1)

;; whitespace mode

(setq prelude-clean-whitespace-on-save nil
      whitespace-style '(face lines-tail trailing)
      whitespace-line-colum 132)


;;(require 'helm-config)

;; icicles

;(setq icicle-define-alias-commands-flag nil)

;(prelude-require-package 'icicles)
;(iswitchb-default-keybindings)
;(prelude-require-package 'icicles-iswitchb)

;(icy-mode)


;;;; PRELUDE

;; Turn off things I don't like from the standard Prelude setup
(ido-mode nil)
(electric-pair-mode -1)
(custom-set-variables


;;;; Customization

 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
