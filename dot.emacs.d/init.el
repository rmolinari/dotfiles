;;;; STARTING CONFIGURATION

;; Initial frame size
(setq inhibit-startup-message t)
(tool-bar-mode -1)

;; Starting directory
(setq default-directory (getenv "HOME"))

;; Typeface
(set-face-font 'default "Inconsolata\-dz-12")
(add-to-list 'default-frame-alist
	     '(font . "Inconsolata\-dz-12"))

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

(blink-cursor-mode -1)

(when window-system (set-frame-size (selected-frame) 180 55))
(load-theme 'solarized-dark t)
;; (load-theme 'wombat t)
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

;; General TeX mode stuff
(setq font-latex-fontify-script nil ; don't set super and subscript stuff in smaller raised/lowered face
      fill-column 132
)

;;; Much more configuration, from http://wiki.contextgarden.net/AUCTeX

;;; Sanjoy Mahajan (sanjoy@mrao.cam.ac.uk), 2006-04-20.  No copyright.
;;;
;;; With recent AUCTeX (11.50 or later), editing ConTeXt files should
;;; just work, but I use the following elisp as well.

; the AUCTeX manual recommends these settings
(setq TeX-parse-self t)			; Enable parse on load.
(setq TeX-auto-save t)			; Enable parse on save.

; for outline views (hide/show sections, chapters, etc.)
(add-hook 'TeX-mode-hook '(lambda () (TeX-fold-mode 1)))
(add-hook 'TeX-mode-hook '(lambda () (outline-minor-mode 1)))
; make PDF by default (can toggle with C-c C-t C-p
(add-hook 'TeX-mode-hook '(lambda () (TeX-PDF-mode 1)))
; these math abbrevs (` as prefix char) are also useful in TeX/ConTeXt files
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)
; Emacs help for \label, \ref, \cite.  Normally used only with
; LaTeX-mode but also useful with plain TeX + eplain and with ConTeXt, so:
(setq reftex-plug-into-AUCTeX t)
(add-hook 'TeX-mode-hook 'reftex-mode)

(defun insert-balanced (left right)
  "Insert a left, right delmiter pair and be poised to type inside them."
  (interactive)
  (insert left)
  (save-excursion
    (insert right)))

; When start-context-math() is bound to $:
; Typing one $ gets you $$ with the insertion point between them.
; Typing a second $ turns the $$ into ConTeXt's form for displayed math:
;
;   \placeformula\startformula
;   [blank line with insertion point at beginning]
;   \stopformula
;
; Delete the \placeformula if you don't want equations numbered automatically.

(defun start-context-math ()
  (interactive)
  (let* ((start (max (point-min) (- (point) 1)))
	 (stop  (min (point-max) (+ (point) 1))))
					; if in the middle of a $$, turn inline math into context display math
    (if (equal "$$" (buffer-substring-no-properties start stop))
	(progn
	  (delete-region start stop)	;get rid of the $$
	  ; delete preceding spaces, if any
	  (while (and (< (point-min) (point))
		      (equal (buffer-substring-no-properties (- (point) 1)
							     (point))
			     " "))
	    (backward-delete-char 1))
	  ; delete a preceding newline, if any
	  (if (equal (buffer-substring-no-properties (- (point) 1)
						     (point))
		     "\n")
	      (backward-delete-char 1))
	  ; ConTeXt's display math with automatic equation numbering
	  ;;(insert "\n\\placeformula\\startformula\n")
	  (insert "\n\\startformula\n")
	  (save-excursion (insert "\n\\stopformula")))
      ; else: just doing inline math
      (insert-balanced ?\$ ?\$))))

; automatically insert right delimiter for $, {, [, and ( and be
; poised to type inside them.
(add-hook 'TeX-mode-hook
	  '(lambda ()
	     (local-set-key "$" 
			    '(lambda ()
			       (interactive)
			       (insert-balanced ?\$ ?\$)))
	     (local-set-key "{"
			    '(lambda ()
			       (interactive)
			       (insert-balanced ?\{ ?\})))
	     (local-set-key "["
			    '(lambda ()
			       (interactive)
			       (insert-balanced ?\[ ?\])))
	     (local-set-key "("
			    '(lambda ()
			       (interactive)
			       (insert-balanced ?\( ?\))))))

; For ConTeXt mode, inserting two $ signs needs to behave specially
(add-hook 'ConTeXt-mode-hook
	  '(lambda ()
	     (local-set-key "$" 'start-context-math)))


;;;; MODES

;; Org mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; iswitch: completion in buffer selection
; (iswitchb-mode 1)

;;; Ruby and related modes

;; Treetop

(add-to-list 'load-path (expand-file-name "~/.emacs.d/treetop-mode"))

(autoload 'treetop-mode "treetop-mode" "Major mode for treetop files" t)
(add-to-list 'auto-mode-alist '("\\.treetop$" . treetop-mode))
(add-to-list 'interpreter-mode-alist '("treetop" . treetop-mode))


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
 '(background-color "#002b36")
 '(background-mode dark)
 '(blink-cursor-mode nil)
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(foreground-color "#839496")
 '(safe-local-variable-values (quote ((TeX-master . t) (eval sh-set-shell "zsh"))))
 '(visible-cursor nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
