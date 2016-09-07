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

;; No tabs
(setq-default indent-tabs-mode nil)

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
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
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

;; Gnus
(setq gnus-init-file "~/.emacs.d/gnus.el")

;; BBDB
(eval-after-load 'bbdb
  '(define-key bbdb-mode-map "r" 'bbdb-merge-records))

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

;;; Haskell

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)


;; IDO

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(icomplete-mode t)

;; whitespace mode

(setq prelude-clean-whitespace-on-save nil
      whitespace-style '(face lines-tail trailing)
      whitespace-line-colum 132)


;;; Others

;; Convert vcf contacts file to bbdb format.  See https://github.com/redguardtoo/gmail2bbdb
;;
;; Example usage:
;;
;;  (gmail2bbdb-import-file "~/Downloads/contacts.vcf")
(add-to-list 'load-path "~/.emacs.d/gmail2bbdb")
(autoload 'gmail2bbdb-import-file "gmail2bbdb")

;;;; NOTES

;;; Some things to try or at least think about

;; helm
;; icicles - too intrusive?

;;; Rust setup
;; Based on https://bassam.co/emacs/2015/08/24/rust-with-emacs/
;; Enable company globally for all mode
(global-company-mode)

(setq company-idle-delay 0.2            ;; Reduce the time after which the company auto completion popup opens
      company-minimum-prefix-length 1   ;; Reduce the number of characters before company kicks in
      racer-cmd "/Users/rory/bin/racer" ;; Set path to racer binary
      racer-rust-src-path "/Users/rory/.rust/src/" ;; Set path to rust src directory
      )

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Setting up configurations when you load rust-mode
(add-hook 'rust-mode-hook
     '(lambda ()
        ;; Enable racer
        (racer-activate)
        ;; Hook in racer with eldoc to provide documentation
        (racer-turn-on-eldoc)
        ;; Use flycheck-rust in rust-mode
        (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
        ;; Use company-racer in rust mode
        (set (make-local-variable 'company-backends) '(company-racer))
        ;; Key binding to jump to method definition
        (local-set-key (kbd "M-.") #'racer-find-definition)
        ;; Key binding to auto complete and indent
        (local-set-key (kbd "TAB") #'racer-complete-or-indent)))


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
 '(canlock-password "504835d1a2afc6972fc882e1ebb6ca7fc31f83f5")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(fci-rule-color "#073642")
 '(foreground-color "#839496")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files
   (quote
    ("~/games/learned_league/opp_research/data/one_day_questions.org" "~/games/learned_league/opp_research/data/off_season.org" "~/games/learned_league/opp_research/data/question_history.org")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values (quote ((TeX-master . t) (eval sh-set-shell "zsh"))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(visible-cursor nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
