;;; Code:

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

; Whitespace (global)
(global-whitespace-mode t)  ;; configured via customize
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Package manager
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list
   'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))


(use-package magit
  :init (global-set-key "\M-?" 'magit-status)
  :ensure t)


(use-package markdown-mode :ensure t)



; my old list of packages
(defvar td-packages
  '(column-marker
    flycheck
    window-number
    yaml-mode ))



;(global-flycheck-mode t)



;; Python

; 72 char marker for docstrings / comments
; whitespace-mode handles highlighting for line too long
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 72)))



;; Abbrevs
(setq-default abbrev-mode t)
(define-abbrev-table 'python-mode-abbrev-table'(
  ("ipdbst" "import ipdb; ipdb.set_trace()" nil 1)
  ("ipdbpm" "import sys,ipdb; ipdb.post_mortem(sys.exc_info()[2])" nil 1)
))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(whitespace-style (quote (face empty tabs lines-tail trailing))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(server-start)
;;; init.el ends here
