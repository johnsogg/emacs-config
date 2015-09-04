(require 'go-mode)

hello

; Make sure $GOPATH/bin is in the executable path, since many of the
; bindings use them.
(setenv "PATH" (concat (getenv "PATH") ":$HOME/Projects/go/bin"))
(setq exec-path (append exec-path '("/Users/johnsogg/Projects/go/bin")))

(setenv "GOPATH" "/Users/johnsogg/Projects/go")

(require 'go-autocomplete)
(require 'auto-complete-config)

(add-to-list 'load-path "/Users/johnsogg/Projects/go/src/github.com/dougm/goflymake")
(require 'go-flymake)


(load "/Users/johnsogg/Projects/go/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
(add-hook 'go-mode-hook 'go-oracle-mode)


(defun my-go-mode-hook ()
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
           "go generate && go build -v && go test -v && go vet"))
					; Tab settings
  (setq tab-width 2)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-]") 'next-error)
  (local-set-key (kbd "M-[") 'previous-error)
  (local-set-key (kbd "M-P") 'recompile)
  (local-set-key (kbd "M-p") 'compile))
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'auto-complete-mode)
