;; Gabe Johnson's .emacs file, most of which was stolen, plundered,
;; and plagarized from various people on the Intertron.
;;
;; An updated version may or may not be found at http://six11.org.

;; Add some things to the load path

(tool-bar-mode -1)
(scroll-bar-mode -1)
(load-theme 'wombat t)
(setq inhibit-startup-screen t)
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(set-default-font "Monaco 12")
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(setq tab-width 4)

(require 'package)
;; (add-to-list 'package-archives 
;;     '("marmalade" .
;;       "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives
;             '("melpa" . "https://melpa.org/packages/"))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                     ("marmalade" . "https://marmalade-repo.org/packages/")
                     ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))

;; uniquify: ensures two buffers are easiliy distinguished by
;; appending contending buffers with their file directory. Eg. if you
;; have /foo/views.py and /bar/views.py buffers open, the buffers will
;; be renamed views.py:foo and views.py:bar. Happy.
(require 'uniquify) 
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(global-set-key "\M-c" 'copy-region-as-kill)
(global-set-key "\M-v" 'yank)
(global-set-key "\M-g" 'goto-line)

(setenv "PATH"
	(concat
	 "/usr/local/bin" ":"
	 (getenv "PATH")))

(global-set-key "\M-!" 'texshop-typeset)
(global-set-key "\M-@" 'texshop-bibtex)

(defun texshop-typeset ()
  (interactive)
  (let ((docname (expand-file-name (buffer-file-name))))
    (do-applescript 
     (format "tell application \"TeXShop\" to open_for_externaleditor at \"%s\"" docname))
    (do-applescript 
     (format "tell application \"TeXShop\" to typeset document \"%s\"" 
	     (file-name-nondirectory docname)))))

(defun texshop-bibtex ()
  (interactive)
  (let ((docname (expand-file-name (buffer-file-name))))
    (do-applescript 
     (format "tell application \"TeXShop\" to open_for_externaleditor at \"%s\"" docname))
    (do-applescript 
     (format "tell application \"TeXShop\" to bibtex document \"%s\"" 
	     (file-name-nondirectory docname)))))

;; Let Emacs use the path environment variable that the shell uses.
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$"
;;                           ""
;;                           (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq eshell-path-env path-from-shell) ; for eshell users
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (when window-system (set-exec-path-from-shell-PATH))

;; Golang stuff

(load "~/.emacs.d/go.el")
; (add-hook 'before-save-hook 'gofmt-before-save)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (fill-column-indicator column-marker color-theme-solarized tidy yaml-mode typescript-mode rainbow-mode popup-complete omnisharp markdown-mode idomenu handlebars-mode go-autocomplete exec-path-from-shell company-go auto-highlight-symbol)))
 '(standard-indent 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "light green" :foreground "textColor" :underline t)))))

(define-globalized-minor-mode 
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
       (text-scale-set 1)
       (kill-local-variable 'text-scale-mode-amount)
       (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
       (global-text-scale-mode 1)
       )

(global-set-key (kbd "M-0")
		'(lambda () (interactive)
		   (global-text-scale-adjust (- text-scale-mode-amount))
		   (global-text-scale-mode -1)))
(global-set-key (kbd "M-=")
		'(lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M--")
		'(lambda () (interactive) (global-text-scale-adjust -1)))
(put 'upcase-region 'disabled nil)
