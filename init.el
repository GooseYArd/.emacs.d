;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
 			 ("marmalade" . "http://marmalade-repo.org/packages/")
 			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))

(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents) (package-install 'markdown-mode))

(unless (package-installed-p 'go-mode)
  (package-refresh-contents) (package-install 'go-mode))

(unless (package-installed-p 'erlang)
  (package-refresh-contents) (package-install 'erlang))

(unless (package-installed-p 'yaml-mode)
  (package-refresh-contents) (package-install 'yaml-mode))

(unless (package-installed-p 'ido)
  (package-refresh-contents) (package-install 'ido))

(unless (package-installed-p 'org)
  (package-refresh-contents) (package-install 'org))

(unless (package-installed-p 'flx-ido)
  (package-refresh-contents) (package-install 'flx-ido))

(unless (package-installed-p 'dtrt-indent)
  (package-refresh-contents) (package-install 'dtrt-indent))

(unless (package-installed-p 'magit)
  (package-refresh-contents) (package-install 'magit))

(setq org-archive-location "~/Dropbox/archive.org::From %s")

;; (toggle-debug-on-error t)

(setq load-path (cons "~/.emacs.d" load-path))
(setq load-path (cons "~/.emacs.d/elisp" load-path))
(setq load-path (cons "~/.emacs.d/elisp/edts" load-path))

(setq load-path (cons  "/usr/local/Cellar/erlang/R16B03/emacs" load-path))
(setq erlang-root-dir "/usr/local/Cellar/erlang/R16B03")
(setq exec-path (cons "/usr/local/Cellar/erlang/R16B03/bin" exec-path))
(require 'erlang-start)

;; (require 'edts-start)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(setq visible-bell t)

(setq-default buffer-file-coding-system 'undecided-unix)

;; (load "/opt/local/share/emacs/site-lisp/haskell-mode-2.4/haskell-site-file")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'font-lock-mode)
;; (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;;; turn on syntax highlighting
(global-font-lock-mode 1)

(autoload 'gtags-mode "gtags" "" t)
(setq c-mode-hook
      '(lambda ()
	 (gtags-mode 1)
	 ))
(setq pascal-mode-hook
      '(lambda ()
	 (gtags-mode 1)
	 ))
(setq java-mode-hook
      '(lambda ()
	 (gtags-mode 1)
	 ))

(add-hook 'gtags-mode-hook 
	  (lambda()
	    (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
	    (local-set-key (kbd "M-,") 'gtags-find-rtag)))  ; reverse tag

(defun djcb-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (let ((olddir default-directory)
	    (topdir (read-directory-name  
		     "gtags: top of source tree:" default-directory)))
	(cd topdir)
	(shell-command "gtags && echo 'created tagfile'")
	(cd olddir)) ; restore   
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'")))

(setq tags-file-name '"~/TAGS")

;;(require 'etags-update)

;; (require 'vc-p4)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(setq org-todo-keywords
      '((type "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|" "CANCELLED(c)" "DEFERRED(e)" "DONE(d)")
	(sequence "PROJECT(p)" "|" "FINISHED(f)")
	(sequence "INVOICE(i)" "SENT(n)" "|" "RCDV(r)")))

(which-function-mode t)

;;(require 'linum)
;;(linum-mode 1) 
;;(add-hook 'find-file-hook (lambda () (linum-mode 1)))
;;(setq linum-format "%d ")

;; Global Settings

(global-set-key "\M-g" 'goto-line) 
(global-set-key "\C-h" 'backward-delete-char)
(define-key function-key-map [delete] [deletechar])

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

;;
;; Languages
;;

;; perl
;;(defun modify-alist (alist-symbol key value &optional search-cdr)
;;      (let ((alist (symbol-value alist-symbol)))
        ;; (while alist
        ;;   (if (eq (if search-cdr
        ;;               (cdr (car alist))
        ;;             (car (car alist))) key)
        ;;       (setcdr (car alist) value)
        ;;     (setq alist (cdr alist))))))
    
;;(modify-alist 'interpreter-mode-alist 'perl-mode 'cperl-mode t)
;;(modify-alist 'auto-mode-alist        'perl-mode 'cperl-mode t)

'(add-hook 'cperl-mode-hook 'cperl-toggle-abbrev)
'(add-hook 'cperl-mode-hook 'cperl-toggle-electric)
(setq cperl-auto-newline nil)
(setq cperl-invalid-face (quote off)) 
(setq cperl-electric-parens nil)
(setq cperl-electric-linefeed nil)


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
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((erlang-indent-level . 4))))
 '(transient-mark-mode (quote identity)))

; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

; tango-dark is not bad

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

(put 'upcase-region 'disabled nil)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;;;(when
;;;    (load
;;;     (expand-file-name "~/.emacs.d/elpa/package.el"))
;;;  (package-initialize))

