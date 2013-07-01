(setq vc-handled-backends nil) 

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; (toggle-debug-on-error t)

(setq load-path (cons "~/.emacs.d" load-path))
(setq load-path (cons "~/.emacs.d/elisp" load-path))
(setq load-path (cons "~/.emacs.d/elisp/mmm-mode-0.5.1" load-path))
(setq load-path (cons "~/.emacs.d/elisp/psgml-1.3.2" load-path))
(setq load-path (cons "~/.emacs.d/elisp/xxml-master" load-path))


(setq load-path (cons "~/.emacs.d/elisp/org-mode/EXPERIMENTAL" load-path))
(setq load-path (cons "~/.emacs.d/elisp/org-mode/lisp" load-path))


(setq visible-bell t)

(setq-default buffer-file-coding-system 'undecided-unix)


;; CF BULLSHIT PEN
(require 'mmm-auto)

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t )
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
    
(setq auto-mode-alist
      (append
       (list
        '("\\.sgm$" . sgml-mode)
        '("\\.sgml$" . sgml-mode)
        '("\\.xml$" . xml-mode)
        )
       auto-mode-alist))

(autoload 'xxml-mode-routine "xxml")
(add-hook 'sgml-mode-hook 'xxml-mode-routine)
(add-hook 'xml-mode-hook 'xxml-mode-routine)

(define-derived-mode sgml-html-mode sgml-mode "HTML"
  "This version of html mode is just a wrapper around sgml mode."
  (make-local-variable 'sgml-declaration)
  (make-local-variable 'sgml-default-doctype-name)
  (make-local-variable 'imenu-sort-function)
  (setq
   sgml-default-doctype-name    "html"
					;   sgml-declaration             "~/lib/DTD/html401/HTML4.decl"
   sgml-always-quote-attributes t
   sgml-indent-step             2
   sgml-indent-data             nil
   sgml-minimize-attributes     nil
   sgml-omittag                 nil
   sgml-shorttag                nil
   imenu-generic-expression   '(("Includes" "<cfinclude[ ]*template=\"\\([a-zA-Z0-9-_./#]*\\)\"[ ]*>" 1)
				("Cases" "<cfcase[ ]*value=\"\\([a-zA-Z0-9-_,. ]*\\)\".*>" 1)
				("Forms" "<form.*action[ ]*=[ ]*\"\\([a-zA-Z0-9-_#.\?=]*\\)\".*>" 1)
				("Queries" "<cfquery.*name[ ]*=[ ]*\"\\([a-zA-Z0-9-_#.]*\\)\".*>" 1)
				("Loops" "<\\(cfloop\\|cfoutput\\).*\\(list\\|to\\|query\\)[ ]*=[ ]*\"\\([a-zA-Z0-9-_#.=]*\\)\".*>" 3)
				("JSFunctions" "function[ ]*\\([a-zA-Z0-9-_]*\\)[ ]*(.*)" 1)
				("Content" "<cfcontent.*type=\"\\([a-zA-Z0-9-_./]*\\)\".*>" 1)
					;cfmodule, cflocation, cftransaction, cfabort, cfif?, add cfform to forms, cflock
				)
   imenu-sort-function 'imenu--sort-by-name
   imenu-auto-rescan t
   )
  )
;; multi-line html comments!
    (defvar lastpos 0)
    (defun check-comment2 ()
      (setq lastpos (point))
      t)
    
    (defun count-matches2 (count exp)
      (let ((x (search-backward exp lastpos t)))
        (if x (count-matches2 (+ 1 count) exp)
          count)))
    (defun count-matches3 (exp p)
      (save-excursion
        (goto-char p)
        (count-matches2 0 exp)))
    
    (defun check-comment ()
      (save-match-data
        (save-excursion
          (let ((x (count-matches3 "--->" (- (point) 4)))
		    (y (count-matches3 "<!---" (point))))
					;(message "%d %d" x y)
	    (if (eq x y) t nil)))))


 (mmm-add-group
     'fancy-html
     '(
             (html-javascript-embedded
                    :submode javascript-generic-mode
                    :face mmm-code-submode-face
                    :front "<script\[^>\]*>"
                    :back "</script>")
             (html-css-embedded
                    :submode css-mode
                    :face mmm-declaration-submode-face
                    :front "<style\[^>\]*>"
                    :back "</style>")
             (html-comment-embedded        ; multi-line html comments!
    ;                :submode text-mode
                    :face font-lock-comment-face
                    :front "<!---"
                    :back "--->"
                    :front-verify check-comment2
                    :back-verify check-comment
                    :include-front
                    :include-back)
             (html-javascript-attribute
                    :submode javascript-generic-mode
                    :face mmm-code-submode-face
                    :front "\\bon\\w+=\\s-*\""
                    :back "\"")
             (html-css-attribute
                    :submode css-mode
                    :face mmm-declaration-submode-face
                    :front "\\bstyle=\\s-*\""
                    :back "\"")
       )
    )
    
    (setq mmm-submode-decoration-level 2)

(setq auto-mode-alist
      (append
       (list
        '("\\.cfm$" . sgml-html-mode)
        '("\\.cfc$" . sgml-html-mode)
        )
       auto-mode-alist))



;; END CF BULLSHIT PEN


;;; turn on syntax highlighting
(global-font-lock-mode 1)

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
;; (add-hook 'groovy-mode-hook
;;           '(lambda ()
;;              (require 'groovy-electric)
;;              (groovy-electric-mode)))


(defun p4-open-file-for-this-buffer ()
  "Call p4 open on the file associated with the current buffer"
  (interactive)
  (let ((p4-file-name (file-name-nondirectory buffer-file-name)))
    (shell-command (format "p4 open %s" p4-file-name))
    (setq buffer-read-only nil)
    ))


(autoload 'cfml-helper-mode "cfml-helper-mode" "Yay HTML" t)
(add-to-list 'auto-mode-alist '("\.cfm$" . cfml-helper-mode))

(autoload 'gtags-mode "gtags" "" t)
(setq c-mode-hook
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

;;(setq tags-file-name
;;           '"~/TAGS")

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

(fset 'noreview
   "Code review: not needed since this is a minor change\C-e")
(global-set-key (kbd "C-c n") 'noreview)

(global-set-key (kbd "C-c c") 'comment-region)


;; Global Settings

(global-set-key "\M-g" 'goto-line) 
(global-set-key "\C-h" 'backward-delete-char)
(define-key function-key-map [delete] [deletechar])

(setq font-lock-maximum-decoration t)
(setq compile-command "make")

(modify-frame-parameters nil '((wait-for-wm . nil)))

(show-paren-mode t)

;; visual stuff when in x11 mode
;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

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

;; (setq compilation-finish-function
;;       (lambda (buf str)
;;         (if (string-match "exited abnormally" str)
;;            ;;there were errors
;;             (message "compilation errors, press C-x ` to visit")
;;           ;;no errors, make the compilation window go away in 0.5 seconds
;;           (run-at-time 0.5 nil 'delete-windows-on buf)
;;           (message "NO COMPILATION ERRORS!"))))

;; (c-toggle-hungry-state 1)

;;
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
 '(transient-mark-mode (quote identity)))

; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

(add-hook 'shell-mode-hook 'n-shell-mode-hook)
(defun n-shell-mode-hook ()
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  )

;;
;; Timestamp insertion crap, I use these to generate incident logs
;;
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert "==========\n")
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n")
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n")
       )

(global-set-key "\C-c\C-d" 'insert-current-date-time)
(global-set-key "\C-c\C-t" 'insert-current-time)

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
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
