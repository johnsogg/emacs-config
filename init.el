;; High level aesthetic stuff
(tool-bar-mode -1)                  ; Disable the button bar atop screen
(scroll-bar-mode -1)                ; Disable scroll bar
(setq inhibit-startup-screen t)     ; Disable startup screen with graphics
(set-default-font "Monaco 12")      ; Set font and size
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(setq tab-width 2)                  ; Two spaces is a tab
(setq visible-bell nil)             ; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   ; Disable super annoying audio bell

;; Make keyboard bindings not suck
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(global-set-key "\M-c" 'copy-region-as-kill)
(global-set-key "\M-v" 'yank)
(global-set-key "\M-g" 'goto-line)

;; Highlight paren mode
(setq show-paren-style 'expression) ; Alternatives are 'parenthesis, 'mixed
(show-paren-mode 1)
(set-face-background 'show-paren-match (face-background 'default))
; (set-face-foreground 'show-paren-match "#ddeeff")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; Set up package repositories so M-x package-install works.
(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Revert buffers when they change on disk but the buffer has no edits.
;; Useful for changing git branches.
(global-auto-revert-mode t)

;; Neotree 
(global-set-key [f8] 'neotree-toggle) ; keybinding is global
(setq neo-smart-open t)               ; find current file on open


;; Color theme
;; (load-theme 'johnsogg-dark t)         ; My colors for optimal rad.
(load-theme 'johnsogg-light t)

;; Add a directory to the load path so we can put extra files there
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Snag the user's PATH and GOPATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt actually invokes goimports
  ;; (if (not (string-match "go" compile-command))   ; change compile command default
  ;;     (set (make-local-variable 'compile-command)
  ;;          "go build"))
  (set (make-local-variable 'compile-command) (format "make -f %s" (get-closest-pathname)))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers
  
  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-s-.") 'godef-jump-other-window) ; Same, use other window
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile command
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  ;; Misc go stuff
  (setq tab-width 2)                              ; Somehow gets overwritten?

)

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Ensure the go specific autocomplete is active in go-mode.
(with-eval-after-load 'go-mode
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (ac-config-default))

;; If the go-guru.el file is in the load path, this will load it.
(require 'go-guru)

;; Projectile mode lets you switch projects and has useful hooks for
;; doing things like set your GOPATH when switching projects.
(projectile-mode)
(global-set-key (kbd "M-f") 'projectile-ag)
(global-set-key (kbd "M-o") 'projectile-find-file)

;; Use reasonable keybindings to change font size for all buffers.
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


;; Make the compilation window automatically disappear - from enberg on #emacs
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "2 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No Compilation Errors!")))))

;; Projectile hook for project switching.
(defun my-switch-project-hook ()
  (go-set-project))
(add-hook 'projectile-after-switch-project-hook #'my-switch-project-hook)

;; From https://superuser.com/questions/455331/emacs-how-to-re-mark-a-previously-marked-region
;; - First establish the bounds of the intitial region (mark -- point).
;; - Then press F6 to set markers to the bounds of that region.
;; - Do whatever you need to do...
;; - Re-establish the bounds of the modified region by pressing C-F6.Repeat *"Do.." as need be
;; - When you have finished use C-S-F6 to clear the region markers.
(global-set-key (kbd "<f6>") 'set-markers-for-region)
(defun set-markers-for-region ()
  (interactive)
  (make-local-variable 'm1)
  (make-local-variable 'm2)
  (setq m1 (copy-marker (mark)))
  (setq m2 (copy-marker (point))))

(global-set-key (kbd "<C-f6>") 'set-region-from-markers)
(defun set-region-from-markers ()
  (interactive)
  (set-mark m1)
  (goto-char m2))

(global-set-key (kbd "<C-S-f6>") 'unset-region-markers)
(defun unset-region-markers ()
  (interactive)
  (set-marker m1 nil)
  (set-marker m2 nil))

;; Semi-non-shitty scrolling
(require 'smooth-scroll) 
(smooth-scroll-mode 1)


(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name file
		      (loop 
			for d = default-directory then (expand-file-name ".." d)
			if (file-exists-p (expand-file-name file d))
			return d
			if (equal d root)
			return nil))))

;; -------------- Everything below here is automatically added by programs -----


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#ffffff" "#183691" "#969896" "#a71d5d" "#969896" "#969896" "#795da3" "#969896"])
 '(custom-safe-themes
   (quote
    ("74e24f29db785033bcf866f54026a3afa56e68ad96bc400f12bd8146b1f37647" "40737e67fbe410a7b852a2eceb33b95ac5a72da81fccfcdc75e16483e60efe38" "a3acbb36707e44e06a01bff579bef51ecfab0fb25c2116a105adb472694ec2e7" "b22d77fcb0d9aff943edd86396fca7ff6da45c42ba115db4e7d585e3804aa5d7" "f5b3f02e37afb5938edb84c05300c15b4d33bd0f449716c7e2eeabb56554c4b9" "4b54f7fd220c061c1c798d8744029df4ec4419b5929e1a5c76f2e0f9e2b1af9a" "98d209aa47b458a73196be9c51611c82b74f7c11672169dc792b4652c0a81da4" "5ea108b0b623979465fee5db9071e0052e0b3c0c9d018e507391e14315fbed3b" "c4d8f0fd2b69194deac8aabaddf8b89517629fd9b76c0b08350762a79058bb2e" "c4a236af4ed91fbb5dc3f10f954210ee4e2d6a524ab02ae4bdb957db7acb8b73" "16a78dd28b8cca6a9ae00cbf47feef88389bbd10647f1185618fa20b1bebb5ae" "4ed33a2def96b6ed3510d461a20bad9a0c33414a59715bbbf09be77ccde7211c" default)))
 '(package-selected-packages
   (quote
    (git-gutter git ag flx-ido projectile rainbow-mode smooth-scroll emojify yaml-mode neotree multi-compile markdown-mode go-rename go-autocomplete github-theme flymake-go exec-path-from-shell atom-one-dark-theme)))
 '(safe-local-variable-values
   (quote
    ((projectile-project-test-cmd . "go test $(go list github.com/bslate/... | grep -v vendor)")
     (projectile-project-compilation-cmd . "go install $(go list github.com/bslate/... | grep -v vendor)")
     (projectile-project-test-cmd . "fooooo")
     (projectile-project-compilation-cmd . "cd ~/Projects/go/src/github.com/bslate ; go install $(go list ./... | grep -v vendor)"))))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
