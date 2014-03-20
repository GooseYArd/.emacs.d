;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))
;; (require 'appearance)


(column-number-mode)

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
 			 ("marmalade" . "http://marmalade-repo.org/packages/")
 			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package)) 
           (package-install package))))
 '(dtrt-indent
   desktop
   erlang
   flx-ido
   git-commit-mode
   go-mode
   guide-key
   ibuffer
   ido
   ido-vertical-mode
   magit
   markdown-mode
   org
   scala-mode2
   windmove
   yaml-mode))

(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(global-set-key (kbd "C-c m") 'magit-status)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(annoying-arrows-mode)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r", "C-x 4", "C-x v", "C-x 8"))
(guide-key-mode 1)
(setq guide-key/recursize-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

(setq org-archive-location "~/Dropbox/archive.org::From %s")
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

;; (toggle-debug-on-error t)

(setq load-path (cons "~/.emacs.d" load-path))
(setq load-path (cons "~/.emacs.d/elisp" load-path))
(setq load-path (cons  "/usr/local/Cellar/erlang/R16B03/emacs" load-path))
(setq erlang-root-dir "/usr/local/Cellar/erlang/R16B03")
(setq exec-path (cons "/usr/local/Cellar/erlang/R16B03/bin" exec-path))

(setq erlang-indent-level 4)

(require 'erlang-start)
(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(setq visible-bell t)
(setq-default buffer-file-coding-system 'undecided-unix)

(global-font-lock-mode 1)

;;; in X, paste at the cursor position, not the mouse position
(setq mouse-yank-at-point t)
(setq scroll-bar-mode -1)

(setq tags-file-name '"~/TAGS")

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(if window-system (setq confirm-kill-emacs 'yes-or-no-p))

(setq org-todo-keywords
      '((type "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|" "CANCELLED(c)" "DEFERRED(e)" "DONE(d)")
	(sequence "PROJECT(p)" "|" "FINISHED(f)")
	(sequence "INVOICE(i)" "SENT(n)" "|" "RCDV(r)")))

(which-function-mode t)


;; Global Settings

(global-set-key "\M-." 'find-tag)
(global-set-key "\M-g" 'goto-line) 
(global-set-key "\C-h" 'backward-delete-char)
(define-key function-key-map [delete] [deletechar])

(setq visual-line-mode t)
(setq font-lock-maximum-decoration t)
(setq compile-command "make")

(modify-frame-parameters nil '((wait-for-wm . nil)))

(show-paren-mode t)

;;
;; Modes
;;

;; Post Mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("sup\\.\\(compose\\|forward\\|reply\\|resume\\)-mode$" . post-mode))
(load "~/.emacs.d/elisp/post.el")

;; point saving
(setq save-place-file "~/.emacs.d/saveplace") 
(setq-default save-place t)
(require 'saveplace)

;; ido
(require 'ido)
(require 'flx-ido)
(require 'ido-vertical-mode)

(setq ido-use-faces nil)
(ido-vertical-mode 1)
(ido-mode 'both) ;; for buffers and files

(setq 
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ido-enable-flex-matching nil     ; don't try to be too smart
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-max-directory-size 100000)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-<DELETE>") 'backward-kill-word)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-formats '((mark modified read-only " " filename-and-process)))

;;
;; Languages
;;

;; C 

(fmakunbound 'c-mode)
(makunbound  'c-mode-map)
(fmakunbound 'c++-mode)
(makunbound  'c++-mode-map)
(makunbound  'c-style-alist)
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)
(autoload 'objc-mode "cc-mode" "Objective-C Editing Mode" t)
(autoload 'java-mode "cc-mode" "Java Editing Mode" t)

(setq auto-mode-alist
  (append
    '(("\\.C$"    . c++-mode)
      ("\\.H$"    . c++-mode)
      ("\\.cc$"   . c++-mode)
      ("\\.hh$"   . c++-mode)
      ("\\.c$"    . c-mode)
      ("\\.h$"    . c-mode)
      ("\\.m$"    . objc-mode)
      ("\\.java$" . java-mode)
     ) auto-mode-alist))

(global-font-lock-mode 1)
(global-set-key [(f9)] 'compile)
(setq compilation-window-height 8)

(add-hook 'c-mode-common-hook 
   (lambda()
     (require 'dtrt-indent)
     (dtrt-indent-mode t)))

(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; end languages
;;

;;
;; shell mode customizations

; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

(load-theme 'manoj-dark t)

(add-hook 'shell-mode-hook 'n-shell-mode-hook)
(defun n-shell-mode-hook ()
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(comint-scroll-to-bottom-on-output t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(org-support-shift-select t)
 '(safe-local-variable-values (quote ((erlang-indent-level . 4))))
 '(transient-mark-mode (quote identity)))

(put 'upcase-region 'disabled nil)
