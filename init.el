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
;;   * Neither the name of the copyright holder(s) nor the names of any
;;     contributors may be used to endorse or promote products derived from
;;     this software without specific prior written permission.
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
(setq package-enable-at-startup nil
      byte-compile--use-old-handlers nil)
(add-hook 'after-init-hook 'init-el-after-init)

;; TODO: remove when Emacs 24.4 is released
(defmacro init-el-with-eval-after-load (file &rest body)
  (declare (indent defun) (debug t))
  `(eval-after-load ,file
     `(,(lambda () ,@body))))

(defun init-el-after-init ()
  (init-el-start-server)
  (init-el-tune-gc)
  (init-el-set-window-size)
  (init-el-disable-useless-gui-stuff)
  (init-el-start-with-empty-scratch-buffer)
  (init-el-disable-backup-files)
  (init-el-enable-fucking-utf-8)
  (init-el-fix-scrolling)
  (init-el-enable-system-clipboard)
  (init-el-change-undo-limits)
  (init-el-enable-all-commands)
  (init-el-initialize-packages)
  (init-el-enable-uniquify)
  (init-el-enable-line-numbers)
  (init-el-enable-undo-tree)
  (init-el-enable-evil)
  (init-el-enable-ido)
  (init-el-enable-search-highlight)
  (init-el-enable-emmet)
  (init-el-setup-ignore-completion-case)
  (init-el-setup-paren-matching)
  (init-el-setup-fonts)
  (init-el-setup-syntax-highlighting)
  (init-el-setup-dabbrev)
  (init-el-setup-auto-complete)
  (init-el-setup-haskell-mode)
  (init-el-setup-keyfreq)
  (init-el-setup-rainbow-delimiters)
  (init-el-setup-smartparens)
  (init-el-setup-indentation)
  (init-el-setup-mappings)
  (init-el-setup-mode-line)
  (init-el-setup-title-bar))

(defun init-el-start-server ()
  (when (eq system-type 'windows-nt)
    (server-start)))

(defun init-el-tune-gc ()
  ;; The default setting is too conservative on modern machines making Emacs
  ;; spend too much time collecting garbage in alloc-heavy code.
  (setq gc-cons-threshold (* 8 1024 1024)))

(defun init-el-set-window-size ()
  (push '(width . 130) default-frame-alist)
  (push '(height . 45) default-frame-alist))

(defun init-el-disable-useless-gui-stuff ()
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (setq use-file-dialog nil
        use-dialog-box nil))

(defun init-el-start-with-empty-scratch-buffer ()
  (fset 'display-startup-echo-area-message (lambda ()))
  (setq inhibit-splash-screen t
        initial-scratch-message ""
        initial-major-mode 'fundamental-mode))

(defun init-el-disable-backup-files ()
  (setq make-backup-files nil
        auto-save-default nil))

(defun init-el-enable-fucking-utf-8 ()
  (prefer-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (setq locale-coding-system 'utf-8)
  (when (eq system-type 'windows-nt)
    (set-clipboard-coding-system 'utf-16le))
  (setq-default buffer-file-coding-system 'utf-8-unix))

(defun init-el-fix-scrolling ()
  (setq mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse 1
        scroll-step 1
        scroll-margin 3
        scroll-conservatively 100000
        scroll-preserve-screen-position 1))

(defun init-el-enable-system-clipboard ()
  (setq mouse-drag-copy-region nil
        x-select-enable-clipboard t)
  (setq-default select-active-regions nil)
  (when (boundp 'x-select-enable-primary)
    (setq x-select-enable-primary nil)))

(defun init-el-change-undo-limits ()
  (setq undo-limit 1048576
        undo-strong-limit 1572864
        undo-outer-limit 20971520))

(defun init-el-enable-all-commands ()
  (setq disabled-command-function nil))

(defun init-el-initialize-packages ()
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (when (or (eq system-type 'windows-nt)
            (/= 0 (user-uid)))
    (let ((refreshed nil))
      (dolist (package '(auto-complete
                         colorsarenice-theme
                         emmet-mode
                         evil
                         fasm-mode
                         haskell-mode
                         highlight-defined
                         htmlize
                         ido-ubiquitous
                         keyfreq
                         litable
                         markdown-mode
                         nlinum
                         number-font-lock-mode
                         php-mode
                         rainbow-delimiters
                         rainbow-mode
                         smartparens
                         smex
                         surround
                         undo-tree))
        (unless (package-installed-p package)
          (unless refreshed
            (package-refresh-contents)
            (setq refreshed t))
          (package-install package))))))

(defun init-el-enable-uniquify ()
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward))

(defun init-el-enable-line-numbers ()
  (global-nlinum-mode))

(defun init-el-enable-undo-tree ()
  (global-undo-tree-mode))

(defun init-el-enable-evil ()
  (evil-mode)
  (setq evil-default-cursor t
        evil-want-fine-undo t)
  (evil-define-operator evil-destroy (beg end type)
    "Delete text from BEG to END with TYPE. Do not save it."
    (evil-delete beg end type ?_ nil))
  (evil-define-motion evil-smart-beginning-of-line ()
    "Workaround to make `smart-beginning-of-line' work in visual
line mode."
    :type exclusive
    (smart-beginning-of-line))
  (global-surround-mode))

(defun init-el-enable-ido ()
  (setq ido-enable-flex-matching t
        smex-save-file (expand-file-name ".smex-items" user-emacs-directory)
        ido-save-directory-list-file (expand-file-name ".ido.last" user-emacs-directory))
  (ido-mode)
  (ido-ubiquitous-mode)
  (init-el-with-eval-after-load 'smex
    (smex-initialize)))

(defun init-el-enable-search-highlight ()
  (setq search-highlight t
        query-replace-highlight t))

(defun init-el-enable-emmet ()
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(defun init-el-setup-ignore-completion-case ()
  (setq completion-ignore-case t
        read-file-name-completion-ignore-case t))

(defun init-el-setup-paren-matching ()
  (show-paren-mode)
  (setq show-paren-delay 0))

(defun init-el-setup-fonts ()
  (if (eq system-type 'windows-nt)
      (cond
       ((find-font (font-spec :name "Consolas"))
        (set-face-attribute 'default nil :family "Consolas" :height 100))
       ((find-font (font-spec :name "Lucida Console"))
        (set-face-attribute 'default nil :family "Lucida Console" :height 100)))
    (set-face-attribute 'default nil :family "Monospace" :height 102)))

(defun init-el-setup-syntax-highlighting ()
  (global-font-lock-mode)
  (global-hl-line-mode)
  (init-el-setup-theme)
  (add-hook 'prog-mode-hook 'number-font-lock-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
  (init-el-with-eval-after-load 'highlight-defined
    (set-face-attribute 'highlight-defined-builtin-function-name-face nil
                        :inherit 'font-lock-type-face)
    (set-face-attribute 'highlight-defined-macro-name-face nil
                        :inherit 'font-lock-preprocessor-face)))

(defun init-el-setup-theme ()
  (let ((theme 'colorsarenice-dark))
    (load-theme theme t)
    ;; Without this hook X11 has problems setting the fucking cursor color.
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (when (eq (window-system frame) 'x)
                  (disable-theme theme)
                  (enable-theme theme))))))

(defun init-el-setup-dabbrev ()
  (setq dabbrev-case-replace nil))

(defun init-el-setup-auto-complete ()
  (init-el-with-eval-after-load 'auto-complete
    (require 'auto-complete-config)
    (ac-config-default)
    (setq-default ac-sources
                  '(ac-source-semantic
                    ac-source-words-in-same-mode-buffers
                    ac-source-abbrev))
    (setq ac-auto-start nil)
    ;; Try to run whatever major mode hooks auto-complete installed
    ;; to ensure `ac-sources' is set correctly.
    (let ((hooks (intern-soft (concat (symbol-name major-mode) "-hook"))))
      (when hooks
        (let* ((acpackage (cadr (assq 'auto-complete package-alist)) )
               (acdirectory (file-name-as-directory (package-desc-dir acpackage))))
          (dolist (hook (symbol-value hooks))
            (when (and (symbolp hook)
                       (string-equal acdirectory (file-name-directory (symbol-file hook))))
              (funcall hook))))))))

(defun init-el-setup-haskell-mode ()
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(defun init-el-setup-indentation ()
  (setq-default indent-tabs-mode nil
                tab-width 2
                c-basic-offset 2)
  (init-el-with-eval-after-load 'cc-mode
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'defun-open 0)
    (c-set-offset 'innamespace 0)))

(defun init-el-setup-keyfreq ()
  (setq keyfreq-file (expand-file-name ".keyfreq" user-emacs-directory))
  (keyfreq-mode)
  (keyfreq-autosave-mode))

(defun init-el-setup-rainbow-delimiters ()
  (add-hook 'change-major-mode-after-body-hook
            (lambda ()
              (if (derived-mode-p 'lisp-mode
                                  'emacs-lisp-mode)
                  (rainbow-delimiters-mode)
                (when (bound-and-true-p rainbow-delimiters-mode)
                  (rainbow-delimiters-mode -1))))))

(defun init-el-setup-smartparens ()
  (require 'smartparens-config)
  (smartparens-global-mode)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (setq-default sp-autoskip-closing-pair t)
  (sp-local-pair '(c-mode c++-mode java-mode css-mode php-mode js-mode perl-mode
                          cperl-mode)
                 "{" nil
                 :post-handlers '((init-el-smartparens-create-and-enter-block "RET"))))

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

(defun init-el-setup-mappings ()
  (global-set-key [remap execute-extended-command] 'smex)
  (global-set-key [remap list-buffers] 'ibuffer-other-window)
  (global-set-key [remap isearch-forward] 'isearch-forward-regexp)
  (global-set-key [remap isearch-backward] 'isearch-backward-regexp)
  (global-set-key [remap move-beginning-of-line] 'smart-beginning-of-line)
  (define-key evil-visual-state-map [remap move-beginning-of-line] 'evil-smart-beginning-of-line)
  (define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)
  (define-key evil-insert-state-map (kbd "C-p") 'auto-complete)
  (define-key evil-insert-state-map (kbd "C-e") 'emmet-expand-line)
  (define-key evil-normal-state-map " " 'evil-ex)
  (define-key evil-visual-state-map " " 'evil-ex)
  (define-key evil-normal-state-map ",e" 'find-file)
  (define-key evil-normal-state-map ",w" 'write-file)
  (define-key evil-normal-state-map ",b" 'switch-to-buffer)
  (define-key evil-normal-state-map ",B" 'list-buffers)
  (define-key evil-normal-state-map ",/" 'evil-ex-nohighlight)
  (define-key evil-normal-state-map ",d" 'evil-destroy)
  (define-key evil-visual-state-map ",d" 'evil-destroy)
  (define-key evil-normal-state-map ",a" 'mark-whole-buffer)
  (define-key evil-visual-state-map ",a" 'mark-whole-buffer)
  (global-set-key [(shift up)] 'windmove-up)
  (global-set-key [(shift down)] 'windmove-down)
  (global-set-key [(shift left)] 'windmove-left)
  (global-set-key [(shift right)] 'windmove-right)
  (global-set-key [f7] 'compile))

(eval-when-compile
  (defmacro mode-line-status-list (&rest elements)
    (declare (indent defun))
    `(let ((strings ()))
       ,@(mapcar
          (lambda (elt)
            (let* ((predicate (pop elt))
                   (string (pop elt))
                   (face (pop elt)))
              (unless (stringp string)
                (error "Expected string, got %s" (type-of string)))
              `(when ,predicate
                 (push (eval-when-compile (propertize ,string 'face ,face)) strings))))
          (reverse elements))
       (mapconcat 'identity strings ","))))

(defun init-el-setup-mode-line ()
  (setq-default
   mode-line-format
   (list
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
    "] ["
    `(:eval (,(lambda ()
                (propertize (symbol-name buffer-file-coding-system)
                            'face 'font-lock-builtin-face))))
    "] ["
    `(:eval (,(lambda ()
                (mode-line-status-list
                  ((buffer-modified-p) "Mod" font-lock-warning-face)
                  (buffer-read-only "RO" font-lock-type-face)
                  ((buffer-narrowed-p) "Narrow" font-lock-type-face)
                  (defining-kbd-macro "Macro" font-lock-type-face)))))
    "]")))

(defun init-el-setup-title-bar ()
  (setq icon-title-format (setq frame-title-format "%b [%f] - Emacs")))

(defun smart-beginning-of-line (&optional lineoffset)
  "Move the point to the first non-white character of the current
line. If the point is already there, move to the beginning of the
line instead. With argument LINEOFFSET not nil or 1, behave like
`beginning-of-line' instead."
  (interactive "^p")
  (setq lineoffset (or lineoffset 1))
  (if (= lineoffset 1)
      (let ((oldpos (point)))
        (back-to-indentation)
        (when (= oldpos (point))
          (move-beginning-of-line 1)))
    (move-beginning-of-line lineoffset)))

(defun macroexpand-all-in-region (beg end)
  "Expand all macros in given region and pretty-print the result
into a new buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (user-error "No region")))
  (let* ((expanded (pp-to-string (macroexpand-all (read (buffer-substring-no-properties beg end)))))
         (resultbuf (generate-new-buffer "*Pp Macroexpand Output*")))
    (with-current-buffer resultbuf
      (insert expanded)
      (emacs-lisp-mode)
      (read-only-mode))
    (select-window (display-buffer resultbuf)))
  nil)

(defun uniq-lines (beg end)
  "Remove consecutive duplicate lines in region BEG to END.

When there's no active region, act on the buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (goto-char beg)
    (let ((previousline nil))
      (while (and (< (point) end)
                  (not (eobp)))
        (let* ((bol (point-at-bol))
               (currentline (buffer-substring-no-properties bol (point-at-eol))))
          (if (string-equal previousline currentline)
              (delete-region bol (progn (forward-line 1) (point)))
            (setq previousline currentline)
            (forward-line 1)))))))

(defun create-buffer (name)
  "Create a new buffer with NAME and switch to it.

The buffer starts in `fundamental-mode'."
  (interactive "BBuffer name:")
  (prog1 (switch-to-buffer (get-buffer-create (generate-new-buffer-name name)))
    (fundamental-mode)))
