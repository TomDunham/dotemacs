;; Basic keymaps
; Largly from Steve Yegge
; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-c #") 'comment-dwim)
(global-set-key (kbd "C-<return>") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f5>") 'call-last-kbd-macro)
(global-set-key (kbd "C-c h") 'copy-file-name-to-clipboard)

; no ui for me
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

; Line numbers
(global-linum-mode 1)

; Stop backup~ and #autosave#
(setq make-backup-files nil)
(setq auto-save-default nil)

; Whitespace
(global-whitespace-mode t)  ;; configured via customize
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Package manager
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list
   'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

; my list of packages
(defvar td-packages '(ack magit flymake-cursor column-marker))

; I run this when I start a new install
(defun td-install-packages ()
  "Install my standard packages"
  (interactive)
  (mapcar 'package-install td-packages))



(eval-after-load "flymake"
  '(progn
     (require 'flymake-cursor)))


;; Magit
(global-set-key
     "\M-?"
     (lambda ()
       (interactive)
       (call-interactively 'magit-status)))


;; Python
; Pyflakes
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

; 72 char marker for docstrings / comments
; whitespace-mode handles highlighting for line too long
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 72)))


;; Abbrevs
(setq-default abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
  ("ipdbst" "import ipdb; ipdb.set_trace()" nil 1)
))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(whitespace-style (quote (face empty tabs lines-tail trailing))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(server-start)
