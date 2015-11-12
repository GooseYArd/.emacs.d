;; (setq server-use-tcp t)
;;(toggle-debug-on-quit t)

(setq load-path (cons "~/.emacs.d/elisp" load-path))

(require 'package)
;; https://github.com/jwiegley/use-package
(require 'bind-key)
(require 'use-package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(setq use-package-always-ensure t)
(use-package python-mode)
(use-package elpy)
(use-package highlight-symbol)
(use-package erlang)
(use-package flyspell)
(use-package flymake)
(use-package dtrt-indent)
(use-package flx-ido)
(use-package guide-key)
(use-package ibuffer)
(use-package ido)
(use-package ido-vertical-mode)
(use-package magit)
(use-package markdown-mode)
(use-package annoying-arrows-mode)
(use-package org)
(use-package windmove)
(use-package yaml-mode)

;;   python-pep8

(package-initialize)
(elpy-enable)

;; Global Settings

(setq large-file-warning-threshold 50000000)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)

(if window-system (setq confirm-kill-emacs 'yes-or-no-p))
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

(setq visible-bell t)
(setq-default buffer-file-coding-system 'undecided-unix)

(global-font-lock-mode 1)
(setq mouse-yank-at-point t)
(setq scroll-bar-mode -1)
(setq vc-follow-symlinks 1)

(global-set-key "\C-c\C-d" 'insert-current-date-time)
;;(global-set-key "\C-c\C-t" 'insert-current-time)

(require 'xfrp_find_replace_pairs) ; for replace-pairs-region
(require 'xeu_elisp_util) ; for get-selection-or-unit

(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-h" 'backward-delete-char)
(define-key function-key-map [delete] [deletechar])

(setq font-lock-maximum-decoration t)
(setq compile-command "make")

(modify-frame-parameters nil '((wait-for-wm . nil)))

(add-hook 'elpy-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

;; make emacsclient split windows when visiting multiple files
(defvar server-visit-files-custom-find:buffer-count)
(defadvice server-visit-files
  (around server-visit-files-custom-find
      activate compile)
  "Maintain a counter of visited files from a single client call."
  (let ((server-visit-files-custom-find:buffer-count 0))
    ad-do-it))
(defun server-visit-hook-custom-find ()
  "Arrange to visit the files from a client call in separate windows."
  (if (zerop server-visit-files-custom-find:buffer-count)
      (progn
    (delete-other-windows)
    (switch-to-buffer (current-buffer)))
    (let ((buffer (current-buffer))
      (window (split-window)))
      (switch-to-buffer buffer)
      (balance-windows)))
  (setq server-visit-files-custom-find:buffer-count
    (1+ server-visit-files-custom-find:buffer-count)))
(add-hook 'server-visit-hook 'server-visit-hook-custom-find)


;;
;; Modes
;;

(add-to-list 'auto-mode-alist '("\\.mak.in\\'" . makefile-mode))

;; ;; erlang
;; ;;(require 'erlang-start)
;; (setq erlang-indent-level 4)
;; (add-to-list 'auto-mode-alist '("\\.config$" . erlang-mode))
;; '(safe-local-variable-values (quote ((erlang-indent-level . 4))))
;; ;;(setq erlang-root-dir "/usr/local/lib/erlang")

;; (if
;;     (not (boundp 'erlang-root-dir))
;;     (message "Skipping erlang-mode: erlang-root-dir not defined. To hook up erlang mode, set erlang-root-dir in your .emacs file before the call to 'require my-config'.")
;;   (progn
;;     (set 'erlang-bin (concat erlang-root-dir "/bin/"))
;;     (set 'erlang-lib (concat erlang-root-dir "/lib/"))
;;     (if
;;         (not (boundp 'erlang-mode-path))
;;         (set 'erlang-mode-path
;;              (concat
;;               erlang-lib
;;               (file-name-completion "tools-" erlang-lib)
;;               "emacs/erlang.el")))
;;     (if
;;         (and
;;          (file-readable-p erlang-mode-path)
;;          (file-readable-p erlang-bin))
;;         (progn
;;           (message "Setting up erlang-mode")
;;           (set 'exec-path (cons erlang-bin exec-path))
;;           (set 'load-path (cons
;;                            (concat
;;                             erlang-lib
;;                             (file-name-completion "tools-" erlang-lib)
;;                             "emacs")
;;                            load-path))
;;           (set 'load-path (cons (file-name-directory erlang-mode-path) load-path))
;;           (require 'erlang-start)
;;           (require 'erlang-flymake)
;;           (require 'erlang-eunit)

;;           (add-hook 'erlang-mode-hook
;;                     (lambda ()
;;                       (setq inferior-erlang-machine-options
;;                             '(
;;                               "-sname" "emacs"
;;                               "-pz" "ebin deps/*/ebin apps/*/ebin"
;;                               "-boot" "start_sasl"
;;                               ))
;;                       (imenu-add-to-menubar "imenu"))))
;;       (message "Skipping erlang-mode: %s and/or %s not readable" erlang-bin erlang-mode-path)
;;       )
;;     )
;;   )
;; (provide 'erlang)

;; misc builtin minors
;;(which-function-mode t)
(show-paren-mode t)
(setq visual-line-mode t)
(column-number-mode 1)


(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; org-mode
(setq org-archive-location "~/archive.org::From %s")
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
(setq org-todo-keywords
      '((type "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|" "CANCELLED(c)" "DEFERRED(e)" "DONE(d)")
        (sequence "PROJECT(p)" "|" "FINISHED(f)")
        (sequence "INVOICE(i)" "SENT(n)" "|" "RCDV(r)")))


;; etags-update
(require 'etags-update)
(etags-update-mode 0)
;;(setq etu/append-file-action 'add)
;;(setq tags-revert-without-query 't)

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r", "C-x 4", "C-x v", "C-x 8"))
(guide-key-mode 1)
(setq guide-key/recursize-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; annoying-arrows
(annoying-arrows-mode 1)

;; magit
(global-set-key (kbd "C-c m") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Post Mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("sup\\.\\(compose\\|forward\\|reply\\|resume\\)-mode$" . post-mode))
;;(load "~/.emacs.d/elisp/post.el")

;; point saving
;; (setq save-place-file "~/.emacs.d/saveplace")
;; (setq-default save-place t)
;; (require 'saveplace)

(setq-default show-trailing-whitespace t)


;; ido
(require 'ido)
(require 'flx-ido)
(require 'ido-vertical-mode)
(setq ido-use-faces nil)
(ido-vertical-mode 1)
(ido-mode t)
;; (ido-mode 'both) ;; for buffers and files

(setq
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
     "^\*compilation" "^\*TAGS" "^session\.*" "^\*")
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

(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-max-directory-size 100000)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-never-show-regexps '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*compilation" "^\*TAGS" "^session\.*" "^\*"))
(setq ibuffer-formats '((mark modified read-only " " (name 18 18 :left :elide) " "  filename-and-process)))

(global-set-key (kbd "M-<DELETE>") 'backward-kill-word)
(global-set-key (kbd "M-_") 'whitespace-cleanup)


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

(add-hook 'c-mode-common-hook
            (lambda()
                  (c-set-offset 'inextern-lang 0)))

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

(when (eq system-type 'darwin)

  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "M+ 1m")
  (custom-set-faces '(default ((t (:height 120 :family "M+ 1m")))))

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  (set-face-attribute 'default nil :height 180)

  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  ;;(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  ;; you may want to add different for other charset in this way.
  )

(setq c-default-style "linux"
      c-basic-offset 8)
(setq-default c-basic-offset 8)

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

;;(load-theme 'manoj-dark t)
;;(load-theme 'whiteboard t)

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
 '(edts-man-root "~/.emacs.d/edts/doc/R15B03-1")
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(org-support-shift-select t)
 '(transient-mark-mode (quote identity)))

(put 'upcase-region 'disabled nil)

;; Ignoring electric indentation
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode lisp-mode
                                                      clojure-mode    scheme-mode
                                                      haskell-mode    ruby-mode
                                                      rspec-mode      python-mode
                                                      c-mode          c++-mode
                                                      objc-mode       latex-mode
                                                      plain-tex-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))

(defun pylogsnip ()
  "insert some log handling crap"
  (interactive)
  (insert-file-contents "~/.emacs.d/python_log_snippet")
  )

(defun xml-remove-linebreak-literals (beg end)
  (interactive "r")
    (goto-char beg)
    (while (search-forward "\\r\\n" end t)
      (replace-match "\n")))

(defun xml-remove-escaping (beg end)
  (interactive "r")
    (goto-char beg)
    (while (re-search-forward "[\\]" end t)
      (replace-match "")))

(defun xml-format-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (xml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

(defun delete-horizontal-space-forward () ; adapted from `delete-horizontal-space'
  "*Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun backward-delete-char-hungry (arg &optional killp)
  "*Delete characters backward in \"hungry\" mode.
    See the documentation of `backward-delete-char-untabify' and
    `backward-delete-char-untabify-method' for details."
  (interactive "*p\nP")
  (let ((backward-delete-char-untabify-method 'hungry))
    (backward-delete-char-untabify arg killp)))

(defun despace (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

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

(defun escape-quotes ()
  "Replace 「\"」 by 「\\\"」 in current line or text selection."
  (interactive)
  (let* ((bds (get-selection-or-unit 'line))
         (p1 (elt bds 1))
         (p2 (elt bds 2)))
    (replace-pairs-region p1 p2 '(["\"" "\\\""])) ) )

(defun unescape-quotes ()
  "Replace  「\\\"」 by 「\"」 in current line or text selection."
  (interactive)
  (let* ((bds (get-selection-or-unit 'line))
        (p1 (elt bds 1))
        (p2 (elt bds 2)))
    (replace-pairs-region p1 p2 '(["\\\"" "\""])) ) )

(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (cl-labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file) possible-file) ; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r default-directory)))
(let ((my-tags-file (find-file-upwards "TAGS")))
  (when my-tags-file
    (message "Loading tags file: %s" my-tags-file)
    (visit-tags-table my-tags-file)))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun align-ws (start end)
  "Repeat alignment with respect to
     the given regular expression."
  (interactive "r")
  (align-regexp start end (concat "\\(\\s-*\\)" "[[:space:]]") 1 1 t))

(global-set-key "\M-." 'find-tag)
(global-set-key (kbd "C-c DEL") 'backward-delete-char-hungry)
(global-set-key (kbd "C-c d") 'delete-horizontal-space-forward)
(global-set-key (kbd "C-c >") 'indent-region)

(defun delete-leading-whitespace ()
  (interactive)
  (narrow-to-region  (point) (mark))
  (goto-char (point-min))
  (replace-regexp "^[\t ]*" "")
  (widen))

(defun backward-kill-line (arg)
   "Kill ARG lines backward."
   (interactive "p")
   (kill-line (- 1 arg)))

(global-set-key "\C-cu" 'backward-kill-line)
