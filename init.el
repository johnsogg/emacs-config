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
  (if (not (string-match "go" compile-command))   ; change compile command default
      (set (make-local-variable 'compile-command)
           "gb build"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers
  
  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump-other-window) ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile command
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  ;; Misc go stuff
  (setq tab-width 2)                              ; Somehow gets overwritten?
  ;; (auto-complete-mode 1)                         ; Enable auto-complete mode
)

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Ensure the go specific autocomplete is active in go-mode.
;;(with-eval-after-load 'go-mode
;;   (require 'go-autocomplete))

;; If the go-guru.el file is in the load path, this will load it.
(require 'go-guru)

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
    ("16a78dd28b8cca6a9ae00cbf47feef88389bbd10647f1185618fa20b1bebb5ae" "4ed33a2def96b6ed3510d461a20bad9a0c33414a59715bbbf09be77ccde7211c" default)))
 '(package-selected-packages
   (quote
    (smooth-scroll emojify yaml-mode neotree multi-compile markdown-mode go-rename go-autocomplete github-theme flymake-go exec-path-from-shell atom-one-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
