;;;; STARTING CONFIGURATION

;; Initial frame size
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


;;;; PACKAGE MANAGEMENT

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )


;;;; APPEARANCE

(when window-system (set-frame-size (selected-frame) 180 55))
(load-theme 'wombat t)
;; (load-theme 'zenburn t)


;;;; AUCTeX

;; Use mkiv
(eval-after-load 'tex
  '(dolist (x
	    '(("ConTeXt" "context --once --synctex %t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once")
	      ("ConTeXt Full" "context  --synctex %t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion")))
     (add-to-list 'TeX-command-list x)))

;; http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer"))
      TeX-view-program-list '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))

(setq TeX-source-correlate-method 'synctex)


;;;; MODES

;; Org mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; iswitch: completion in buffer selection
; (iswitchb-mode 1)


;; IDO

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(icomplete-mode t)

;; whitespace mode

(setq prelude-clean-whitespace-on-save nil
      whitespace-style '(face lines-tail trailing)
      whitespace-line-colum 132)


;;;; NOTES

;;; Some things to try or at least think about

;; helm
;; icicles - too intrusive?


;;;; LAST THINGS

(server-start)


;;;; CUSTOMIZATION

(custom-set-variables
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
