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
   'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; local lisp
(add-to-list 'load-path "~/.emacs.d/lisp/")

(use-package ack
  :ensure t)

(use-package browse-kill-ring
  :ensure t)

(use-package cider :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package ggtags
  :ensure t
  :bind (("C-c g s" . ggtags-find-other-symbol)
         ("C-c g h" . ggtags-view-tag-history)
         ("C-c g r" . ggtags-find-reference)
         ("C-c g f" . ggtags-find-file)
         ("C-c g c" . ggtags-create-tags)
         ("C-c g u" . ggtags-update-tags)
         ("M-," . pop-tag-mark)))


(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gotest
  :ensure t)

(use-package go-mode
  :config
  (defun my-go-mode-hook ()
    ;; https://github.com/cockroachdb/cockroach/wiki/Ben's-Go-Emacs-setup
    (local-set-key (kbd "M-.") #'godef-jump)
    (add-hook 'before-save-hook 'gofmt-before-save)
    (let ((map go-mode-map))
        (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
        (define-key map (kbd "C-c m") 'go-test-current-file)
        (define-key map (kbd "C-c .") 'go-test-current-test)
        (define-key map (kbd "C-c b") 'go-run)))
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  :ensure t)


(use-package flymake-go
  :ensure t)


(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/popup-window-position 'bottom
          guide-key/guide-key-sequence t  ; enable for all prefixes
          guide-key/recursive-key-sequence-flag t)
    (guide-key-mode 1)))

(use-package js2-mode
  :ensure t)


(use-package magit
  :init (global-set-key "\M-?" 'magit-status)
  :ensure t)

(use-package markdown-mode :ensure t)

(use-package mtail-mode)

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode)
  :config
  (defun slack-php-mode-hook ()
    (setq tab-width 8)
    (setq indent-tabs-mode t)
    (setq c-basic-offset 8)
    ;; slack-style requires the initial indent
    ;; https://stackoverflow.com/a/1136007
    (c-set-offset 'topmost-intro 8)
    (c-set-offset 'cpp-macro -8))
  (add-hook 'php-mode-hook 'slack-php-mode-hook))

(use-package terraform-mode
  :ensure t)

(use-package vagrant-tramp
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" . yaml-mode))


(use-package yapfify
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; if I want col markers again, prob fill-column-indicator
;; like this: https://github.com/bdd/.emacs.d/blob/master/packages.el#L76


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
 '(package-selected-packages
   (quote
    (gotest editorconfig terraform-mode logstash-conf-mode ack flymake-go go-flymake js2-mode ggtags cider go-mode yapfify exec-path-from-shell vagrant-tramp browse-kill-ring yaml-mode guide-key gitignore-mode gitconfig-mode php-mode use-package markdown-mode magit)))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(whitespace-style (quote (face empty tabs lines-tail trailing))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(server-start)


;; should be in own file
;; find source code locations in browser



(defun s5-proj-prefix-p (fn ele)
    (string-prefix-p (substitute-in-file-name (car ele)) fn))

(defun s5-line-no ()
  (let ((start (point-min))
	(n (line-number-at-pos)))
    (if (= start 1)
        n
      (save-excursion
	(save-restriction
	  (widen)
          (+ n (line-number-at-pos start) -1))))))


(defun s5-ghe-url (fn br &optional ln)
  ; url to view source file `fn`, branch `br`, line number `ln`
  (concat
   (format (cdr (seq-find (apply-partially 'proj-prefix-p fn)
                          s5-proj-map))
           br
           (file-relative-name fn
                               (substitute-in-file-name
                                (car (seq-find (apply-partially 'proj-prefix-p fn)
                                               s5-proj-map)))))
   (if ln (format "#L%s" ln) "")))

(defun s5-browse-ghe-url-at-line ()
  (interactive)
  (browse-url (s5-ghe-url
               (buffer-file-name)
               (car (vc-git-branches))
               (s5-line-no))))



;;; init.el ends here
