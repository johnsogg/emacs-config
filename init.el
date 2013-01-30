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

(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(setq visible-bell t)

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
