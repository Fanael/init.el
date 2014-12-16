;;; -*- lexical-binding: t -*-
;; Copyright (c) 2013-2014, Fanael Linithien
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
(setq package-enable-at-startup nil)
(add-hook 'after-init-hook #'init-el-after-init)

(defun init-el-after-init ()
  (init-el-tune-gc)
  (init-el-use-new-byte-code-opcodes)
  (init-el-set-window-size)
  (init-el-disable-useless-gui-stuff)
  (init-el-start-with-empty-scratch-buffer)
  (init-el-setup-fonts)
  (init-el-disable-lock-files)
  (init-el-setup-backup-files)
  (init-el-setup-auto-save)
  (init-el-use-fucking-utf-8)
  (init-el-setup-package-archives)
  (init-el-install-required-packages)
  (init-el-fix-scrolling)
  (init-el-setup-clipboard)
  (init-el-set-undo-limits)
  (init-el-do-not-disable-commands)
  (init-el-disable-electric-indent)
  (init-el-disable-vc)
  (init-el-setup-undo-tree)
  (init-el-setup-ignore-completion-case)
  (init-el-setup-history)
  (init-el-setup-helm)
  (init-el-setup-evil)
  (init-el-setup-surround)
  (init-el-setup-search-highlight)
  (init-el-setup-emmet)
  (init-el-setup-whitespace-mode)
  (init-el-setup-text-mode)
  (init-el-setup-paren-matching)
  (init-el-setup-theme)
  (init-el-setup-line-highlighting)
  (init-el-setup-emacs-lisp-special-form-highlighting)
  (init-el-setup-number-highlighting)
  (init-el-setup-rainbow-identifiers)
  (init-el-setup-highlight-quoted)
  (init-el-setup-company)
  (init-el-setup-anaconda)
  (init-el-setup-slime)
  (init-el-setup-haskell-mode)
  (init-el-setup-rainbow-delimiters)
  (init-el-setup-smartparens)
  (init-el-setup-flycheck)
  (init-el-setup-eldoc)
  (init-el-setup-indentation)
  (init-el-setup-bindings)
  (init-el-setup-mode-line)
  (init-el-setup-title-bar)
  (init-el-setup-buffer-boundary-indicators)
  (init-el-setup-paragraph-filling)
  (init-el-setup-echo-keystrokes)
  (init-el-start-server))

(eval-and-compile
  (defconst init-el-package-archives
    '(("melpa" . "http://melpa.org/packages/")
      ("gnu" . "http://elpa.gnu.org/packages/")))

  (defconst init-el-required-packages
    (append
     (when (eq system-type 'windows-nt)
       '(helm-w32-launcher))
     '(ace-jump-mode
       colorsarenice-theme
       company
       company-anaconda
       company-ghc
       emmet-mode
       evil
       evil-surround
       fasm-mode
       flycheck
       haskell-mode
       helm
       highlight-blocks
       highlight-numbers
       highlight-quoted
       htmlize
       ipretty
       markdown-mode
       package-safe-delete
       php-mode
       rainbow-delimiters
       rainbow-identifiers
       rainbow-mode
       slime
       slime-company
       smartparens
       undo-tree
       yaml-mode)))

  (defun init-el-install-required-packages* ()
    (let ((refreshed nil))
      (dolist (package init-el-required-packages)
        (unless (package-installed-p package)
          (unless refreshed
            (package-refresh-contents)
            (setq refreshed))
          (package-install package))))))

(cl-eval-when (compile)
  (require 'package)
  (let ((package-archives init-el-package-archives))
    (package-initialize)
    (init-el-install-required-packages*)))

(defmacro init-el-require-when-compiling (feature)
  "Require FEATURE only when byte-compiling.
The sole purpose of this macro is to tell the byte-compiler that functions and
variables provided by FEATURE are in scope, so it doesn't warn about them."
  (when (bound-and-true-p byte-compile-current-file)
    (require feature))
  nil)

(defmacro init-el-with-eval-after-load (feature &rest body)
  "Execute BODY after FEATURE is loaded."
  (declare (indent defun) (debug t))
  `(with-eval-after-load ',feature
     (init-el-require-when-compiling ,feature)
     ,@body))

(defmacro init-el-deferred (&rest body)
  (declare (indent defun) (debug t))
  `(run-with-idle-timer 0.1 nil ,(pcase body
                                   (`(#',functionname)
                                    `#',functionname)
                                   (_
                                    `(lambda () ,@body)))))

(defun init-el-tune-gc ()
  ;; The default setting is too conservative on modern machines making Emacs
  ;; spend too much time collecting garbage in alloc-heavy code.
  (setq gc-cons-threshold (* 24 1024 1024)))

(defun init-el-use-new-byte-code-opcodes ()
  (setq byte-compile--use-old-handlers nil))

(defun init-el-set-window-size ()
  (push '(width . 130) default-frame-alist)
  (push '(height . 45) default-frame-alist))

(defun init-el-disable-useless-gui-stuff ()
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (setq use-file-dialog nil
        use-dialog-box nil))

(defun init-el-start-with-empty-scratch-buffer ()
  (fset #'display-startup-echo-area-message #'ignore)
  (setq inhibit-splash-screen t
        initial-scratch-message ""
        initial-major-mode #'fundamental-mode))

(defun init-el-setup-fonts ()
  (if (eq system-type 'windows-nt)
      (cond
       ((find-font (font-spec :name "Consolas"))
        (set-face-attribute 'default nil :family "Consolas" :height 100))
       ((find-font (font-spec :name "Lucida Console"))
        (set-face-attribute 'default nil :family "Lucida Console" :height 100)))
    (set-face-attribute 'default nil :family "Monospace" :height 100)))

(defun init-el-disable-lock-files ()
  (setq create-lockfiles nil))

(defun init-el-setup-backup-files ()
  (setq backup-by-copying t
        delete-old-versions t
        kept-old-versions 3
        kept-new-versions 7
        version-control t)
  (let ((backupdir (expand-file-name "backups" user-emacs-directory)))
    (setq backup-directory-alist (list (cons "." backupdir)))))

(defun init-el-setup-auto-save ()
  (let ((autosavedir (file-name-as-directory (expand-file-name "autosave" user-emacs-directory))))
    (setq auto-save-list-file-prefix (expand-file-name ".saves-" autosavedir))
    (setq auto-save-file-name-transforms (list (list ".*" (replace-quote autosavedir) t)))))

(defun init-el-use-fucking-utf-8 ()
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (setq locale-coding-system 'utf-8)
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8))
  (setq-default buffer-file-coding-system 'utf-8-unix))

(defun init-el-setup-package-archives ()
  (setq package-archives init-el-package-archives)
  (package-initialize))

(defun init-el-install-required-packages ()
  (when (or (eq system-type 'windows-nt)
            (/= 0 (user-uid)))
    (init-el-install-required-packages*)))

(defun init-el-fix-scrolling ()
  (setq mouse-wheel-progressive-speed nil
        scroll-margin 3
        scroll-conservatively 100000
        scroll-preserve-screen-position 'always))

(defun init-el-setup-clipboard ()
  (setq-default select-active-regions nil)
  (when (boundp 'x-select-enable-primary)
    (setq x-select-enable-primary nil)))

(defun init-el-set-undo-limits ()
  (setq undo-limit (* 16 1024 1024)
        undo-strong-limit (* 24 1024 1024)
        undo-outer-limit (* 64 1024 1024)))

(defun init-el-do-not-disable-commands ()
  (setq disabled-command-function nil))

(defun init-el-disable-electric-indent ()
  (when (bound-and-true-p electric-indent-mode)
    (electric-indent-mode -1)))

(defun init-el-disable-vc ()
  (setq vc-handled-backends '()))

(defun init-el-setup-undo-tree ()
  (global-undo-tree-mode)
  (init-el-require-when-compiling undo-tree)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-lazy-drawing nil
        undo-tree-auto-save-history t)
  (let ((undodir (expand-file-name "undo" user-emacs-directory)))
    (setq undo-tree-history-directory-alist (list (cons "." undodir)))))

(defun init-el-setup-ignore-completion-case ()
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t))

(defun init-el-setup-history ()
  (setq history-length 1024
        search-ring-max 1024
        regexp-search-ring-max 1024)
  (savehist-mode)
  (init-el-require-when-compiling savehist)
  (setq savehist-additional-variables '(search-ring
                                        regexp-search-ring)
        savehist-file (expand-file-name ".savehist" user-emacs-directory)))

(defun init-el-setup-helm ()
  (init-el-deferred
    (cl-letf (((symbol-function #'message) #'ignore))
      (helm-mode)
      (init-el-require-when-compiling helm)
      (setq helm-move-to-line-cycle-in-source t
            helm-prevent-escaping-from-minibuffer nil)))
  (init-el-with-eval-after-load helm-command
    (setq helm-M-x-always-save-history t)))

(defun init-el-setup-evil ()
  (evil-mode)
  (init-el-require-when-compiling evil)
  (setq evil-want-fine-undo t
        evil-echo-state nil
        evil-ex-substitute-global t)
  (evil-define-operator evil-destroy (beg end type)
    "Delete text from BEG to END with TYPE. Do not save it."
    (evil-delete beg end type ?_ nil))
  (evil-set-command-properties
   #'smart-beginning-of-line :repeat 'motion :type 'exclusive :keep-visual t))

(defun init-el-setup-surround ()
  (global-evil-surround-mode))

(defun init-el-setup-search-highlight ()
  (setq search-highlight t
        query-replace-highlight t))

(defun init-el-setup-emmet ()
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode))

(defun init-el-setup-whitespace-mode ()
  (init-el-with-eval-after-load whitespace
    (setq whitespace-style '(face trailing lines-tail empty space-before-tab))))

(defun init-el-setup-text-mode ()
  (add-hook 'text-mode-hook #'visual-line-mode))

(defun init-el-setup-paren-matching ()
  (show-paren-mode)
  (init-el-require-when-compiling paren)
  (setq show-paren-delay 0))

(defun init-el-setup-theme ()
  (let ((theme 'colorsarenice-dark))
    (load-theme theme t)
    (unless (eq system-type 'windows-nt)
      ;; Without this hook the cursor color is not set correctly under X11.
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (when (eq (window-system frame) 'x)
                    (disable-theme theme)
                    (enable-theme theme)))))))

(defun init-el-setup-line-highlighting ()
  (global-hl-line-mode))

(defun init-el-setup-emacs-lisp-special-form-highlighting ()
  (add-hook 'emacs-lisp-mode-hook #'init-el-highlight-all-special-forms))

(defun init-el-highlight-all-special-forms ()
  (let ((regexp
         (eval-when-compile
           (let ((specialforms ()))
             (mapatoms (lambda (symbol)
                         (when (fboundp symbol)
                           (let ((fn (symbol-function symbol)))
                             (when (and (subrp fn)
                                        (eq 'unevalled (cdr (subr-arity fn))))
                               (push (symbol-name symbol) specialforms))))))
             (concat "(" (regexp-opt specialforms t) "\\_>")))))
    (font-lock-add-keywords nil
                            `((,regexp (1 'font-lock-keyword-face))))))

(defun init-el-setup-number-highlighting ()
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(defun init-el-setup-rainbow-identifiers ()
  (init-el-with-eval-after-load rainbow-identifiers
    (setq rainbow-identifiers-choose-face-function #'rainbow-identifiers-cie-l*a*b*-choose-face
          rainbow-identifiers-faces-to-override '(highlight-quoted-symbol))))

(defun init-el-setup-highlight-quoted ()
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
  (add-hook 'lisp-mode-hook #'highlight-quoted-mode))

(defun init-el-setup-company ()
  (init-el-deferred
    (global-company-mode)
    (init-el-require-when-compiling company)
    (let ((it company-backends))
      (while it
        (let ((backend (car it)))
          (when (memq backend '(company-elisp company-capf))
            (setcar it `(,backend :with company-dabbrev-code))))
        (setq it (cdr it))))
    (setq company-idle-delay nil
          company-selection-wrap-around t
          company-require-match nil))
  (init-el-with-eval-after-load company-dabbrev
    (setq company-dabbrev-minimum-length 3
          company-dabbrev-other-buffers t)))

(defun init-el-setup-anaconda ()
  (add-hook 'python-mode-hook #'anaconda-mode)
  (init-el-with-eval-after-load company
    (init-el-with-eval-after-load python
      (add-to-list 'company-backends #'company-anaconda))))

(defun init-el-setup-slime ()
  (init-el-with-eval-after-load slime
    (setq inferior-lisp-program "sbcl"))
  (add-hook 'lisp-mode-hook #'init-el-setup-slime-first-time))

(defun init-el-setup-slime-first-time ()
  (slime-setup '(slime-asdf
                 slime-autodoc
                 slime-company
                 slime-editing-commands
                 slime-fancy-inspector
                 slime-fancy-trace
                 slime-fontifying-fu
                 slime-highlight-edits
                 slime-package-fu
                 slime-references
                 slime-repl
                 slime-trace-dialog
                 slime-xref-browser))
  (remove-hook 'lisp-mode-hook #'init-el-setup-slime-first-time))

(defun init-el-setup-haskell-mode ()
  (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook #'ghc-init)
  (init-el-with-eval-after-load company
    (init-el-with-eval-after-load haskell-mode
      (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code)))))

(defun init-el-setup-rainbow-delimiters ()
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(defun init-el-setup-smartparens ()
  (init-el-deferred
    (require 'smartparens-config)
    (smartparens-global-mode)
    (init-el-require-when-compiling smartparens)
    (setq sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil)
    (setq-default sp-autoskip-closing-pair t)
    (sp-local-pair '(c-mode c++-mode java-mode css-mode php-mode js-mode perl-mode
                            cperl-mode)
                   "{" nil
                   :post-handlers '((init-el-smartparens-create-and-enter-block "RET")))))

(defun init-el-smartparens-create-and-enter-block (&rest _)
  (save-excursion
    ;; Indent the line with the opening brace, but only if it
    ;; contains nothing more than the brace.
    (end-of-line 0)
    (let ((oldpoint (point)))
      (back-to-indentation)
      (when (= (1+ (point)) oldpoint)
        (indent-according-to-mode))))
  ;; Open the block and reindent the closing brace.
  (newline)
  (indent-according-to-mode)
  ;; Enter it.
  (forward-line -1)
  (indent-according-to-mode))

(defun init-el-setup-flycheck ()
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (init-el-with-eval-after-load flycheck
    (setq flycheck-idle-change-delay 1)
    (setq-default flycheck-cppcheck-checks '("style" "missingInclude")
                  flycheck-cppcheck-inconclusive t
                  flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))))

(defun init-el-setup-eldoc ()
  (init-el-with-eval-after-load eldoc
    (setq eldoc-idle-delay 0.25)
    (when (fboundp 'global-eldoc-mode)
      (global-eldoc-mode -1))
    (add-hook 'prog-mode-hook #'init-el-enable-eldoc-mode)
    (when (boundp 'eval-expression-minibuffer-setup-hook)
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))))

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

(defun init-el-setup-indentation ()
  (setq-default indent-tabs-mode nil)
  (init-el-with-eval-after-load cc-vars
    (setq-default c-basic-offset 2))
  (init-el-with-eval-after-load haskell-indentation
    (setq haskell-indentation-starter-offset 2))
  (init-el-with-eval-after-load cc-mode
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'defun-open 0)
    (c-set-offset 'innamespace 0)))

(defun init-el-setup-bindings ()
  (global-set-key [remap execute-extended-command] #'helm-M-x)
  (global-set-key [remap list-buffers] #'ibuffer-other-window)
  (global-set-key [remap isearch-forward] #'isearch-forward-regexp)
  (global-set-key [remap isearch-backward] #'isearch-backward-regexp)
  (global-set-key [remap move-beginning-of-line] #'smart-beginning-of-line)
  (global-set-key [remap eval-expression] #'pp-eval-expression)
  (global-set-key [remap eval-last-sexp] #'pp-eval-last-sexp)
  (global-set-key [remap eval-print-last-sexp] #'ipretty-last-sexp)
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
  (define-key evil-motion-state-map ",j" #'evil-ace-jump-word-mode)
  (define-key evil-motion-state-map ",k" #'evil-ace-jump-char-mode)
  (define-key evil-motion-state-map ",l" #'evil-ace-jump-line-mode)
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
  (global-set-key (kbd "C-c r") #'rainbow-identifiers-mode)
  (global-set-key (kbd "C-c b") #'highlight-blocks-now)
  (global-set-key (kbd "C-c m") #'pp-macroexpand-all)
  (global-set-key (kbd "C-c i") #'helm-semantic-or-imenu)
  (when (eq system-type 'windows-nt)
    (let ((keymap (make-sparse-keymap)))
      (define-key keymap "l" #'helm-w32-launcher)
      (define-key keymap "L" #'helm-w32-launcher-elevated)
      (define-key keymap "d" #'helm-w32-launcher-open-shortcut-directory)
      (define-key keymap "p" #'helm-w32-launcher-open-shortcut-properties)
      (define-key keymap "f" #'helm-w32-launcher-flush-cache)
      (global-set-key (kbd "C-c l") keymap)))
  (init-el-with-eval-after-load helm
    (define-key helm-map "\t" #'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") #'helm-select-action)))

(eval-when-compile
  (defmacro init-el-mode-line-status-list (&rest elements)
    `(let ((strings ()))
       ,@(mapcar
          (lambda (elt)
            (pcase elt
              (`(,predicate ,string ,face)
               (unless (stringp string)
                 (error "Expected string, got %s" (type-of string)))
               `(when ,predicate
                  (push (eval-when-compile (propertize ,string 'face ,face)) strings)))
              (_
               (error "unknown mode line status element %S" elt))))
          (reverse elements))
       (mapconcat #'identity strings ","))))

(defun init-el-setup-mode-line ()
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
    `(:eval (,(lambda ()
                (init-el-mode-line-status-list
                 ((buffer-modified-p) "Mod" font-lock-warning-face)
                 (buffer-read-only "RO" font-lock-type-face)
                 ((buffer-narrowed-p) "Narrow" font-lock-type-face)
                 (defining-kbd-macro "Macro" font-lock-type-face)))))
    "]%]")))

(defun init-el-setup-title-bar ()
  (setq icon-title-format (setq frame-title-format "%b [%f] - Emacs")))

(defun init-el-setup-buffer-boundary-indicators ()
  (setq-default indicate-empty-lines t
                indicate-buffer-boundaries 'left))

(defun init-el-setup-paragraph-filling ()
  (setq sentence-end-double-space nil)
  (setq-default fill-column 80))

(defun init-el-setup-echo-keystrokes ()
  (setq echo-keystrokes 5.391063232E-44))

(defun init-el-start-server ()
  (when (eq system-type 'windows-nt)
    (init-el-deferred #'server-start)))

(defun smart-beginning-of-line (&optional lineoffset)
  "Move the point to the first non-white character of the current line.
If the point is already there, move to the beginning of the line instead.
With argument LINEOFFSET not nil or 1, move forward LINEOFFSET - 1 lines first."
  (interactive "^p")
  (unless (memq lineoffset '(nil 1))
    (move-beginning-of-line lineoffset))
  (let ((oldpos (point)))
    (back-to-indentation)
    (when (= oldpos (point))
      (move-beginning-of-line 1))))

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
                       (unless (fboundp 'pp-last-sexp)
                         (require 'pp))
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
   (progn
     (barf-if-buffer-read-only)
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list (point-min) (point-max)))))
  (save-excursion
    (goto-char beg)
    (save-restriction
      (narrow-to-region beg end)
      (let ((previousline nil))
        (while (not (eobp))
          (let* ((bol (line-beginning-position))
                 (currentline (buffer-substring-no-properties bol (line-end-position))))
            (forward-line 1)
            (if (string-equal previousline currentline)
                (delete-region bol (point))
              (setq previousline currentline))))))))

(defun open-directory-in-external-browser (directory)
  "Open DIRECTORY in the system's default file browser.

When called interactively, open the directory containing the file visited in the
current buffer, if any; otherwise open `default-directory'."
  (interactive
   (let ((bufferfile (buffer-file-name)))
     (list (if bufferfile
               (file-name-directory bufferfile)
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
           (if (bound-and-true-p server-buffer-clients)
               (progn
                 (init-el-require-when-compiling server)
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
