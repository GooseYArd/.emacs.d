;;
;; Install package from command line. Example:
;;
;;   $ emacs --batch --expr "(define pkg-to-install 'smex)" -l emacs-pkg-install.el
;;

(require 'package)

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(mapc
 (lambda (package)
   (or (package-installed-p package)
           (package-install package))) '(highlight-symbol erlang flyspell python-mode flymake dtrt-indent flx-ido guide-key ibuffer ido ido-vertical-mode magit markdown-mode annoying-arrows-mode org windmove yaml-mode))

;; Fix HTTP1/1.1 problems
(setq url-http-attempt-keepalives nil)

(package-refresh-contents)

(package-install pkg-to-install)
