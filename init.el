;;; init.el --- -*- lexical-binding: t -*-
;; Copyright (c) 2013-2015, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; Commentary:
;;; Code:
(eval-and-compile
  (require 'package)
  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
  (setq package-enable-at-startup nil)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

;;; Tune the GC
;; The default setting is too conservative on modern machines making Emacs
;; spend too much time collecting garbage in alloc-heavy code.
(setq gc-cons-threshold (* 24 1024 1024))

;;; Use new byte codes
(setq byte-compile--use-old-handlers nil)

;;; Set window size
(push '(width . 130) default-frame-alist)
(push '(height . 45) default-frame-alist)

;;; Disable useless GUI stuff
(tool-bar-mode -1)
(scroll-bar-mode -1)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(menu-bar-mode -1)
(blink-cursor-mode -1)
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;;; Start with empty scratch buffer
(fset #'display-startup-echo-area-message #'ignore)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq initial-major-mode #'fundamental-mode)

;;; Set the font
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :family "Consolas" :height 100)
  (set-face-attribute 'default nil :family "Monospace" :height 100))

;;; Disable lock files
(setq create-lockfiles nil)

;;; Backup files
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-old-versions 3)
(setq kept-new-versions 7)
(setq version-control t)
(let ((backup-dir (expand-file-name "backups" user-emacs-directory)))
  (setq backup-directory-alist (list (cons "." backup-dir))))

;;; Auto-save
(let ((auto-save-dir (file-name-as-directory (expand-file-name "autosave" user-emacs-directory))))
  (setq auto-save-list-file-prefix (expand-file-name ".saves-" auto-save-dir))
  (setq auto-save-file-name-transforms (list (list ".*" (replace-quote auto-save-dir) t))))

;;; Use fucking UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(setq-default buffer-file-coding-system 'utf-8-unix)

;;; Fix scrolling
(setq mouse-wheel-progressive-speed nil)
(setq scroll-margin 3)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 'always)

;;; Clipboard
(setq-default select-active-regions nil)
(when (boundp 'x-select-enable-primary)
  (setq x-select-enable-primary nil))

;;; Set undo limits
(setq undo-limit (* 16 1024 1024))
(setq undo-strong-limit (* 24 1024 1024))
(setq undo-outer-limit (* 64 1024 1024))

;;; Do not disable commands
(setq disabled-command-function nil)

;;; Disable electrict indent
(when (bound-and-true-p electric-indent-mode)
  (electric-indent-mode -1))

;;; Disable VC
(setq vc-handled-backends '())

;;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-lazy-drawing nil)
  (setq undo-tree-auto-save-history t)
  (let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
    (setq undo-tree-history-directory-alist (list (cons "." undo-dir)))))

;;; Ignore case for completion
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; History saving
(use-package savehist
  :ensure nil
  :config
  (setq history-length 1024)
  (setq search-ring-max 1024)
  (setq regexp-search-ring-max 1024)
  (setq savehist-additional-variables '(search-ring regexp-search-ring))
  (setq savehist-file (expand-file-name ".savehist" user-emacs-directory))
  (savehist-mode))

;;; Helm
(use-package helm
  :config
  (cl-letf (((symbol-function #'message) #'ignore))
    (helm-mode)
    (helm-autoresize-mode))
  (setq helm-move-to-line-cycle-in-source t)
  (setq helm-prevent-escaping-from-minibuffer nil)
  (setq helm-display-header-line nil)
  (setq helm-split-window-in-side-p t)
  (setq helm-autoresize-min-height 30)
  (setq helm-autoresize-max-height 30)
  (use-package helm-command
    :defer t
    :ensure helm
    :config
    (setq helm-M-x-always-save-history t)))
(use-package helm-swoop
  :defer t)

;;; evil
(use-package evil
  :config
  (evil-mode)
  (setq evil-want-fine-undo t)
  (setq evil-echo-state nil)
  (setq evil-ex-substitute-global t)
  (evil-set-command-properties
   #'smart-beginning-of-line :repeat 'motion :type 'exclusive :keep-visual t))

(evil-define-operator evil-destroy (beg end type)
  "Delete text from BEG to END with TYPE. Do not save it."
  (evil-delete beg end type ?_ nil))

;;; evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode))

;;; Search highlight
(setq search-highlight t)
(setq query-replace-highlight t)

;;; emmet-mode
(use-package emmet-mode
  :defer t
  :init
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode))

;;; dabbrev
(use-package dabbrev
  :ensure nil
  :defer t
  :config
  (setq dabbrev-case-replace nil)
  (setq dabbrev-case-distinction nil))

;;; whitespace-mode
(use-package whitespace
  :ensure nil
  :defer t
  :config
  (setq whitespace-style '(face trailing lines-tail empty space-before-tab)))

;;; text-mode
(add-hook 'text-mode-hook #'visual-line-mode)

;;; show-paren-mode
(show-paren-mode)
(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0))

;;; Set the theme
(use-package colorsarenice-theme
  :defer t)
(let ((theme 'colorsarenice-dark))
  (load-theme theme t)
  (unless (eq system-type 'windows-nt)
    ;; Without this hook the cursor color is not set correctly under X11.
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (when (eq (window-system frame) 'x)
                  (disable-theme theme)
                  (enable-theme theme))))))

;;; Number highlighting in programming modes
(use-package highlight-numbers
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;;; rainbow-identifiers
(use-package rainbow-identifiers
  :defer t
  :config
  (setq rainbow-identifiers-choose-face-function #'rainbow-identifiers-cie-l*a*b*-choose-face)
  (setq rainbow-identifiers-faces-to-override '(highlight-quoted-symbol)))

;;; highlight-quoted
(use-package highlight-quoted
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
  (add-hook 'lisp-mode-hook #'highlight-quoted-mode))

;;; company
(use-package company
  :config
  (global-company-mode)
  (let ((it company-backends))
    (while it
      (let ((backend (car it)))
        (when (memq backend '(company-elisp company-capf))
          (setcar it `(,backend :with company-dabbrev-code))))
      (setq it (cdr it))))
  (setq company-idle-delay nil)
  (setq company-selection-wrap-around t)
  (setq company-require-match nil)
  (use-package company-dabbrev
    :ensure company
    :defer t
    :config
    (setq company-dabbrev-minimum-length 3)
    (setq company-dabbrev-other-buffers t)))

;;; anaconda
(use-package company-anaconda
  :defer t
  :init
  (use-package anaconda-mode
    :defer t
    :init
    (add-hook 'python-mode-hook #'anaconda-mode))
  (use-package python
    :ensure nil
    :defer t
    :config
    (push #'company-anaconda company-backends)))

;;; SLIME
(use-package slime
  :defer t
  :config
  (setq slime-lisp-implementations '((sbcl ("sbcl"))))
  (setq slime-default-lisp 'sbcl)
  (add-hook 'lisp-mode-hook #'init-el-setup-slime-first-time))

(defun init-el-setup-slime-first-time ()
  (slime-setup '(slime-asdf
                 slime-autodoc
                 slime-company
                 slime-editing-commands
                 slime-fancy-inspector
                 slime-fancy-trace
                 slime-fontifying-fu
                 slime-package-fu
                 slime-references
                 slime-repl
                 slime-trace-dialog
                 slime-xref-browser))
  (remove-hook 'lisp-mode-hook #'init-el-setup-slime-first-time))

;;; haskell-mode
(use-package haskell-mode
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (use-package ghc
    :defer t
    :init
    (add-hook 'haskell-mode-hook #'ghc-init))
  :config
  (push '(company-ghc :with company-dabbrev-code) company-backends))

;;; rainbow-delimiters
(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; smartparens
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)
  (setq-default sp-autoskip-closing-pair t)
  (sp-local-pair '(c-mode c++-mode java-mode css-mode php-mode js-mode perl-mode
                          cperl-mode rust-mode)
                 "{" nil
                 :post-handlers '((init-el-smartparens-create-and-enter-block "RET"))))

(defun init-el-smartparens-create-and-enter-block (&rest _)
  (save-excursion
    ;; Indent the line with the opening brace, but only if it
    ;; contains nothing more than the brace.
    (end-of-line 0)
    (let ((old-point (point)))
      (back-to-indentation)
      (when (= (1+ (point)) old-point)
        (indent-according-to-mode))))
  ;; Open the block and reindent the closing brace.
  (newline)
  (indent-according-to-mode)
  ;; Enter it.
  (forward-line -1)
  (indent-according-to-mode))

;;; flycheck
(use-package flycheck
  :defer t
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 1)
  (setq-default flycheck-cppcheck-checks '("style" "missingInclude")
                flycheck-cppcheck-inconclusive t
                flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))

;;; avy
(use-package avy
  :defer t
  :config
  (setq avy-style 'pre)
  (setq avy-keys (eval-when-compile (number-sequence ?a ?z)))
  (setq avy-all-windows nil)
  (setq avy-case-fold-search nil))

;; highlight-blocks
(use-package highlight-blocks
  :defer t)

;;; eldoc
(use-package eldoc
  :ensure nil
  :defer t
  :init
  (setq eldoc-idle-delay 0.25)
  (when (fboundp 'global-eldoc-mode)
    (global-eldoc-mode -1))
  (add-hook 'prog-mode-hook #'init-el-enable-eldoc-mode)
  (when (boundp 'eval-expression-minibuffer-setup-hook)
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)))

(defalias 'init-el-enable-eldoc-mode
  (if (fboundp 'global-eldoc-mode)
      (lambda ()
        (unless (memq eldoc-documentation-function '(ignore nil))
          (eldoc-mode)))
    (lambda ()
      (when (or (not (memq eldoc-documentation-function
                           '(eldoc-documentation-function-default nil)))
                (derived-mode-p #'emacs-lisp-mode))
        (eldoc-mode)))))

;;; Rust
(use-package rust-mode
  :defer t)
(use-package racer
  :defer t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  :config
  (setq racer-rust-src-path (expand-file-name "projects/rust/rustc-nightly/src/" (getenv "HOME"))))

;;; Indentation
(setq-default indent-tabs-mode nil)
(use-package cc-vars
  :ensure nil
  :defer t
  :config
  (setq-default c-basic-offset 2)
  (setq c-auto-align-backslashes nil))
(use-package haskell-indentation
  :ensure haskell-mode
  :defer t
  :config
  (setq haskell-indentation-starter-offset 2))
(use-package cc-mode
  :ensure nil
  :defer t
  :config
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'defun-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inextern-lang 0))

;;; Key bindings
(global-set-key [remap execute-extended-command] #'helm-M-x)
(global-set-key [remap switch-to-buffer] #'helm-buffers-list)
(global-set-key [remap list-buffers] #'ibuffer-other-window)
(global-set-key [remap isearch-forward] #'isearch-forward-regexp)
(global-set-key [remap isearch-backward] #'isearch-backward-regexp)
(global-set-key [remap move-beginning-of-line] #'smart-beginning-of-line)
(global-set-key [remap eval-expression] #'pp-eval-expression)
(global-set-key [remap eval-last-sexp] #'pp-eval-last-sexp)
(define-key evil-insert-state-map (kbd "RET") #'newline-and-indent)
(define-key evil-insert-state-map (kbd "C-<SPC>") #'company-complete)
(define-key evil-insert-state-map (kbd "C-e") #'emmet-expand-line)
(define-key evil-motion-state-map "," nil)
(define-key evil-motion-state-map " " #'evil-repeat-find-char-reverse)
(define-key evil-motion-state-map ",e" #'find-file)
(define-key evil-motion-state-map ",w" #'write-file)
(define-key evil-motion-state-map ",s" #'save-buffer)
(define-key evil-motion-state-map ",q" #'bury-buffer-delete-window-or-frame)
(define-key evil-motion-state-map ",b" #'switch-to-buffer)
(define-key evil-motion-state-map ",B" #'list-buffers)
(define-key evil-motion-state-map ",d" #'evil-destroy)
(define-key evil-motion-state-map ",a" #'mark-whole-buffer)
(define-key evil-motion-state-map ",j" #'evil-avy-goto-word-1)
(define-key evil-motion-state-map ",k" #'evil-avy-goto-char)
(define-key evil-motion-state-map ",l" #'evil-avy-goto-line)
(define-key evil-motion-state-map ",p" #'previous-buffer)
(define-key evil-motion-state-map ",n" #'next-buffer)
(define-key evil-motion-state-map [up] #'evil-previous-visual-line)
(define-key evil-insert-state-map [up] #'evil-previous-visual-line)
(define-key evil-motion-state-map [down] #'evil-next-visual-line)
(define-key evil-insert-state-map [down] #'evil-next-visual-line)
(global-set-key [(shift up)] #'windmove-up)
(global-set-key [(shift down)] #'windmove-down)
(global-set-key [(shift left)] #'windmove-left)
(global-set-key [(shift right)] #'windmove-right)
(add-hook 'org-shiftup-final-hook #'windmove-up)
(add-hook 'org-shiftleft-final-hook #'windmove-left)
(add-hook 'org-shiftdown-final-hook #'windmove-down)
(add-hook 'org-shiftright-final-hook #'windmove-right)
(global-set-key [f7] #'compile)
(global-set-key (kbd "M-/") #'dabbrev-completion)
(global-set-key (kbd "C-M-/") #'dabbrev-expand)
(global-set-key (kbd "C-c r") #'rainbow-identifiers-mode)
(global-set-key (kbd "C-c b") #'highlight-blocks-now)
(global-set-key (kbd "C-c m") #'pp-macroexpand-all)
(global-set-key (kbd "C-c i") #'helm-semantic-or-imenu)
(global-set-key (kbd "C-c s") #'helm-swoop)
(use-package helm
  :defer t
  :config
  (define-key helm-map "\t" #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action))

;; Mode line format
(setq-default
 mode-line-format
 (list
  (eval-when-compile
    (concat
     " "
     (propertize "%b" 'face 'font-lock-keyword-face)
     " ("
     (propertize "%02l" 'face 'font-lock-type-face)
     ","
     (propertize "%02c" 'face 'font-lock-type-face)
     ") ["
     (propertize "%p" 'face 'font-lock-constant-face)
     "/"
     (propertize "%I" 'face 'font-lock-constant-face)
     "] ["
     (propertize "%m" 'face 'font-lock-string-face)
     "] ["))
  `(:eval (,(lambda ()
              (propertize (symbol-name buffer-file-coding-system)
                          'face 'font-lock-builtin-face))))
  "] ["
  `(:eval (,(lambda ()
              (propertize (symbol-name evil-state)
                          'face 'font-lock-function-name-face))))
  "] %[["
  `(:eval
    (,(lambda ()
        (let ((strings '()))
          (cl-macrolet
              ((add-string
                (string face)
                `(push ,(propertize string 'face face) strings)))
            (when defining-kbd-macro
              (add-string "Macro" font-lock-type-face))
            (when (buffer-narrowed-p)
              (add-string "Narrow" font-lock-type-face))
            (when buffer-read-only
              (add-string "RO" font-lock-type-face))
            (pcase overwrite-mode
              (`overwrite-mode-textual
               (add-string "Overwrite" font-lock-warning-face))
              (`overwrite-mode-binary
               (add-string "Bin-overwrite" font-lock-warning-face)))
            (when (buffer-modified-p)
              (add-string "Mod" font-lock-warning-face)))
          (mapconcat #'identity strings ",")))))
  "]%]"))

;;; Title bar format
(setq icon-title-format (setq frame-title-format "%b [%f] - Emacs"))

;;; Enable the buffer boundary indicators
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;;; Paragraph filling
(setq sentence-end-double-space nil)
(setq-default fill-column 80)

;;; Echo keystrokes
(setq echo-keystrokes 5.391063232E-44)

;;; windmove
(use-package windmove
  :ensure nil
  :config
  (setq windmove-wrap-around t))

;;; Customize
;; Allow the code using customize to save their stuff to somewhere else than
;; the init file. Don't load it, though, as I don't use customize.
(setq custom-file (expand-file-name ".custom" user-emacs-directory))

;;; Interactive functions
(defun smart-beginning-of-line (&optional line-offset)
  "Move the point to the first non-white character of the current line.
If the point is already there, move to the beginning of the line instead.
With argument LINE-OFFSET not nil or 1, move forward LINE-OFFSET - 1 lines
first."
  (interactive "^p")
  (unless (memq line-offset '(nil 1))
    (move-beginning-of-line line-offset))
  (let ((old-point (point)))
    (back-to-indentation)
    (when (= old-point (point))
      (move-beginning-of-line 1))))

(autoload 'pp-last-sexp "pp")

(defun pp-macroexpand-all (beg end)
  "Expand all macros in region BEG to END.
The result is shown pretty-printed in a new buffer.
When called interactively with no region active, or from Lisp with BEG nil,
expand all macros in the sexp before point."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     '(nil nil)))
  (let* ((expanded (pp-to-string
                    (macroexpand-all
                     (if beg
                         (read (buffer-substring-no-properties beg end))
                       (pp-last-sexp)))))
         (resultbuf (generate-new-buffer "*Pp Macroexpand Output*")))
    (with-current-buffer resultbuf
      (insert expanded)
      (emacs-lisp-mode)
      (read-only-mode)
      (set-buffer-modified-p nil))
    (select-window (display-buffer resultbuf)))
  nil)

(defun uniq-lines (beg end)
  "Remove consecutive duplicate lines in region BEG to END.

When there's no active region, act on the visible portion of the current
buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (goto-char beg)
    (save-restriction
      (narrow-to-region beg end)
      (let ((previous-line nil))
        (while (not (eobp))
          (let* ((bol (line-beginning-position))
                 (current-line (buffer-substring-no-properties bol (line-end-position))))
            (forward-line 1)
            (if (string-equal previous-line current-line)
                (delete-region bol (point))
              (setq previous-line current-line))))))))

(defun open-directory-in-external-browser (directory)
  "Open DIRECTORY in the system's default file browser.

When called interactively, open the directory containing the file visited in the
current buffer, if any; otherwise open `default-directory'."
  (interactive
   (let ((buffer-file (buffer-file-name)))
     (list (if buffer-file
               (file-name-directory buffer-file)
             default-directory))))
  (if (fboundp 'w32-shell-execute)
      (w32-shell-execute "open" directory nil 1)
    (condition-case nil
        (start-process "" nil "xdg-open" directory)
      (file-error (error "Don't know how to open a directory on this system")))))

(defun bury-buffer-delete-window-or-frame ()
  "Bury the current buffer and delete its window or frame.
When the selected window is the only window belonging to the selected frame, the
frame is deleted; otherwise, the window is deleted.
When the current buffer has client processes requesting its editing, mark the
buffer as \"done\"; note that this may kill the buffer instead of burying it."
  (interactive)
  (let ((frame-to-delete (selected-frame))
        (window-to-delete (selected-window))
        (next-buffer-suggestion
         (let ((buffer-to-bury (current-buffer)))
           (if (and (bound-and-true-p server-buffer-clients)
                    (null (cdr (get-buffer-window-list buffer-to-bury nil t))))
               (progn
                 (eval-when-compile (require 'server))
                 (server-buffer-done buffer-to-bury))
             (bury-buffer buffer-to-bury)
             nil))))
    (cond
     (next-buffer-suggestion
      (let ((next-buffer (car next-buffer-suggestion)))
        (when next-buffer
          (switch-to-buffer next-buffer))))
     ((null (cdr (window-list frame-to-delete nil nil)))
      (delete-frame frame-to-delete))
     (t
      (delete-window window-to-delete)))))
;;; init.el ends here
