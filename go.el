(require 'go-mode)

; Make sure $GOPATH/bin is in the executable path, since many of the
; bindings use them.
(setenv "PATH" (concat (getenv "PATH") ":$HOME/Projects/go/bin"))
(setq exec-path (append exec-path '("/Users/johnsogg/Projects/go/bin")))

(setenv "GOPATH" "/Users/johnsogg/Projects/go")

(require 'go-autocomplete)
(require 'auto-complete-config)

(add-to-list 'load-path "/Users/johnsogg/Projects/go/src/github.com/dougm/goflymake")
(require 'go-flymake)

(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt

  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Use company mode
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))
  ; Customize compile command to build, test, vet.
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "gb build -v  && gb test"))
					; Tab settings
  (setq tab-width 2)
  (set-face-background 'hl-line "#3e4446")
  (set-face-foreground 'highlight nil)
  (set-face-attribute hl-line-face nil :underline nil)

					; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'go-oracle-callees)
  (local-set-key (kbd "M-]") 'next-error)
  (local-set-key (kbd "M-[") 'previous-error)
  (local-set-key (kbd "M-P") 'recompile)
  (local-set-key (kbd "M-p") 'compile)

  (local-set-key (kbd "M-0")
		 '(lambda () (interactive)
		    (global-text-scale-adjust (- text-scale-mode-amount))
		    (global-text-scale-mode -1)))
  (local-set-key (kbd "M-=")
		 '(lambda () (interactive) (global-text-scale-adjust 1)))
  (local-set-key (kbd "M--")
		  '(lambda () (interactive) (global-text-scale-adjust -1)))
)
(add-hook 'go-mode-hook 'my-go-mode-hook)
; (add-hook 'go-mode-hook 'auto-highlight-symbol-mode)
(add-hook 'go-mode-hook 'hl-line-mode)
