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
(setq package-enable-at-startup nil)
(add-hook 'after-init-hook 'init-el-after-init)

(defun init-el-after-init ()
  (init-el-tune-gc)
  (init-el-use-new-byte-code-opcodes)
  (init-el-set-window-size)
  (init-el-disable-useless-gui-stuff)
  (init-el-start-with-empty-scratch-buffer)
  (init-el-disable-lock-files)
  (init-el-setup-backup-files)
  (init-el-setup-auto-save)
  (init-el-use-fucking-utf-8)
  (init-el-fix-scrolling)
  (init-el-setup-clipboard)
  (init-el-set-undo-limits)
  (init-el-do-not-disable-commands)
  (init-el-setup-fonts)
  (init-el-setup-package-archives)
  (init-el-install-required-packages)
  (init-el-disable-electric-indent)
  (init-el-setup-uniquify)
  (init-el-setup-undo-tree)
  (init-el-setup-ido)
  (init-el-setup-history)
  (init-el-setup-evil)
  (init-el-setup-surround)
  (init-el-setup-search-highlight)
  (init-el-setup-emmet)
  (init-el-setup-whitespace-mode)
  (init-el-setup-text-mode)
  (init-el-setup-ignore-completion-case)
  (init-el-setup-paren-matching)
  (init-el-setup-theme)
  (init-el-setup-line-highlighting)
  (init-el-setup-emacs-lisp-special-form-highlighting)
  (init-el-setup-number-highlighting)
  (init-el-setup-rainbow-identifiers)
  (init-el-setup-dabbrev)
  (init-el-setup-auto-complete)
  (init-el-setup-jedi)
  (init-el-setup-haskell-mode)
  (init-el-setup-rainbow-delimiters)
  (init-el-setup-highlight-blocks)
  (init-el-setup-smartparens)
  (init-el-setup-flycheck)
  (init-el-setup-line-numbers)
  (init-el-setup-indentation)
  (init-el-setup-mappings)
  (init-el-setup-mode-line)
  (init-el-setup-title-bar)
  (init-el-setup-buffer-boundary-indicators)
  (init-el-setup-paragraph-filling)
  (init-el-start-server))

(defmacro init-el-with-eval-after-load (file &rest body)
  "Execute BODY after FILE is loaded.

FILE can be a feature or a file name, see `eval-after-load' for
details."
  (declare (indent defun) (debug t))
  ;; Load the file at byte-compile time to avoid spurious warnings.
  (when (bound-and-true-p byte-compile-current-file)
    (unless (if (symbolp file)
                (require file nil :no-error)
              (load file :no-message :no-error))
      (message "init-el-with-eval-after-load: couldn't load %s" file)))
  `(eval-after-load ',file
     `(,(lambda () ,@body))))

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
  (fset 'display-startup-echo-area-message (lambda ()))
  (setq inhibit-splash-screen t
        initial-scratch-message ""
        initial-major-mode 'fundamental-mode))

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
  (set-selection-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (setq locale-coding-system 'utf-8)
  (when (eq system-type 'windows-nt)
    (set-clipboard-coding-system 'utf-16le))
  (setq-default buffer-file-coding-system 'utf-8-unix))

(defun init-el-fix-scrolling ()
  (setq mouse-wheel-progressive-speed nil
        scroll-step 1
        scroll-margin 3
        scroll-conservatively 100000
        scroll-preserve-screen-position 'always))

(defun init-el-setup-clipboard ()
  (setq mouse-drag-copy-region nil
        x-select-enable-clipboard t)
  (setq-default select-active-regions nil)
  (when (boundp 'x-select-enable-primary)
    (setq x-select-enable-primary nil)))

(defun init-el-set-undo-limits ()
  (setq undo-limit (* 16 1024 1024)
        undo-strong-limit (* 24 1024 1024)
        undo-outer-limit (* 64 1024 1024)))

(defun init-el-do-not-disable-commands ()
  (setq disabled-command-function nil))

(defun init-el-setup-fonts ()
  (if (eq system-type 'windows-nt)
      (cond
       ((find-font (font-spec :name "Consolas"))
        (set-face-attribute 'default nil :family "Consolas" :height 100))
       ((find-font (font-spec :name "Lucida Console"))
        (set-face-attribute 'default nil :family "Lucida Console" :height 100)))
    (set-face-attribute 'default nil :family "Monospace" :height 100)))

(defun init-el-setup-package-archives ()
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
  (package-initialize))

(defun init-el-install-required-packages ()
  (when (or (eq system-type 'windows-nt)
            (/= 0 (user-uid)))
    (let ((refreshed nil))
      (dolist (package '(ace-jump-mode
                         auto-complete
                         emmet-mode
                         evil
                         evil-surround
                         fasm-mode
                         flycheck
                         haskell-mode
                         highlight-blocks
                         highlight-numbers
                         htmlize
                         ido-ubiquitous
                         jedi
                         markdown-mode
                         php-mode
                         rainbow-delimiters
                         rainbow-identifiers
                         rainbow-mode
                         relative-line-numbers
                         smartparens
                         smex
                         stekene-theme
                         undo-tree
                         yaml-mode))
        (unless (package-installed-p package)
          (unless refreshed
            (package-refresh-contents)
            (setq refreshed t))
          (package-install package))))))

(defun init-el-disable-electric-indent ()
  (when (bound-and-true-p electric-indent-mode)
    (electric-indent-mode -1)))

(defun init-el-setup-uniquify ()
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward))

(defun init-el-setup-undo-tree ()
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-lazy-drawing nil
        undo-tree-auto-save-history t)
  (let ((undodir (expand-file-name "undo" user-emacs-directory)))
    (setq undo-tree-history-directory-alist (list (cons "." undodir))))
  (global-undo-tree-mode))

(defun init-el-setup-ido ()
  (setq ido-enable-flex-matching t
        ido-auto-merge-work-directories-length -1
        ido-use-filename-at-point 'guess
        ido-default-file-method 'selected-window
        smex-history-length 256
        smex-save-file (expand-file-name ".smex-items" user-emacs-directory)
        ido-save-directory-list-file (expand-file-name ".ido.last" user-emacs-directory))
  (ido-mode)
  (ido-everywhere)
  (ido-ubiquitous-mode)
  (init-el-with-eval-after-load smex
    (smex-initialize)))

(defun init-el-setup-history ()
  (setq history-length 1024
        search-ring-max 1024
        regexp-search-ring-max 1024
        savehist-additional-variables '(search-ring
                                        regexp-search-ring)
        savehist-file (expand-file-name ".savehist" user-emacs-directory))
  (savehist-mode))

(defun init-el-setup-evil ()
  (evil-mode)
  (setq evil-want-fine-undo t
        evil-echo-state nil
        evil-ex-substitute-global t)
  (evil-define-operator evil-destroy (beg end type)
    "Delete text from BEG to END with TYPE. Do not save it."
    (evil-delete beg end type ?_ nil))
  (evil-define-motion evil-smart-beginning-of-line ()
    "Workaround to make `smart-beginning-of-line' work in visual
line mode."
    :type exclusive
    (smart-beginning-of-line)))

(defun init-el-setup-surround ()
  (global-evil-surround-mode)
  (push '(?T . init-el-surround-bbcode-tag) (default-value 'evil-surround-pairs-alist)))

(defun init-el-surround-bbcode-tag ()
  (let ((tagname (read-string "BBCode tag: " nil nil nil t)))
    (cons (format "[%s]" tagname)
          (format "[/%s]" tagname))))

(defun init-el-setup-search-highlight ()
  (setq search-highlight t
        query-replace-highlight t))

(defun init-el-setup-emmet ()
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(defun init-el-setup-whitespace-mode ()
  (setq whitespace-style '(face trailing lines-tail empty space-before-tab)))

(defun init-el-setup-text-mode ()
  (add-hook 'text-mode-hook 'visual-line-mode))

(defun init-el-setup-ignore-completion-case ()
  (setq completion-ignore-case t
        read-file-name-completion-ignore-case t))

(defun init-el-setup-paren-matching ()
  (show-paren-mode)
  (setq show-paren-delay 0))

(defun init-el-setup-theme ()
  (let ((theme 'stekene-dark))
    (load-theme theme t)
    ;; Without this hook X11 has problems setting the fucking cursor color.
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (when (eq (window-system frame) 'x)
                  (disable-theme theme)
                  (enable-theme theme))))))

(defun init-el-setup-line-highlighting ()
  (global-hl-line-mode))

(defun init-el-setup-emacs-lisp-special-form-highlighting ()
  (add-hook 'emacs-lisp-mode-hook 'init-el-highlight-all-special-forms))

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
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(defun init-el-setup-rainbow-identifiers ()
  (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(defun init-el-setup-dabbrev ()
  (setq dabbrev-case-replace nil))

(defun init-el-setup-auto-complete ()
  (ac-config-default)
  (setq ac-auto-start nil
        ac-expand-on-auto-complete nil
        ac-comphist-file (expand-file-name ".ac-comphist" user-emacs-directory)))

(defun init-el-auto-complete ()
  (interactive)
  (unless (local-variable-p 'init-el-in-auto-complete)
    (unwind-protect
        (progn
          (make-local-variable 'init-el-in-auto-complete)
          (unless (bound-and-true-p auto-complete-mode)
            (auto-complete-mode))
          (auto-complete))
      (kill-local-variable 'init-el-in-auto-complete))))

(defun init-el-setup-jedi ()
  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook 'jedi:setup))

(defun init-el-setup-haskell-mode ()
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(defun init-el-setup-rainbow-delimiters ()
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(defun init-el-setup-highlight-blocks ()
  (add-hook 'prog-mode-hook 'highlight-blocks-mode))

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

(defun init-el-setup-flycheck ()
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq flycheck-idle-change-delay 1
        flycheck-completion-system 'ido)
  (setq-default flycheck-cppcheck-checks '("style" "missingInclude")
                flycheck-cppcheck-inconclusive t
                flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))

(defun init-el-setup-line-numbers ()
  (setq relative-line-numbers-format
        (lambda (offset)
          (if (= offset 0)
              "=>"
            (format "%2d" (abs offset)))))
  (global-relative-line-numbers-mode))

(defun init-el-setup-indentation ()
  (setq-default indent-tabs-mode nil
                c-basic-offset 2)
  (init-el-with-eval-after-load cc-mode
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'defun-open 0)
    (c-set-offset 'innamespace 0)))

(defun init-el-setup-mappings ()
  (global-set-key [remap execute-extended-command] 'smex)
  (global-set-key [remap list-buffers] 'ibuffer-other-window)
  (global-set-key [remap isearch-forward] 'isearch-forward-regexp)
  (global-set-key [remap isearch-backward] 'isearch-backward-regexp)
  (global-set-key [remap move-beginning-of-line] 'smart-beginning-of-line)
  (define-key evil-motion-state-map [remap move-beginning-of-line] 'evil-smart-beginning-of-line)
  (define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)
  (define-key evil-insert-state-map (kbd "C-<SPC>") 'init-el-auto-complete)
  (define-key evil-insert-state-map (kbd "C-e") 'emmet-expand-line)
  (define-key evil-normal-state-map " " 'evil-ex)
  (define-key evil-visual-state-map " " 'evil-ex)
  (define-key evil-normal-state-map ",e" 'find-file)
  (define-key evil-normal-state-map ",w" 'write-file)
  (define-key evil-normal-state-map ",b" 'switch-to-buffer)
  (define-key evil-visual-state-map ",b" 'switch-to-buffer)
  (define-key evil-normal-state-map ",B" 'list-buffers)
  (define-key evil-visual-state-map ",B" 'list-buffers)
  (define-key evil-normal-state-map ",d" 'evil-destroy)
  (define-key evil-visual-state-map ",d" 'evil-destroy)
  (define-key evil-normal-state-map ",a" 'mark-whole-buffer)
  (define-key evil-visual-state-map ",a" 'mark-whole-buffer)
  (define-key evil-motion-state-map "," nil)
  (define-key evil-motion-state-map ",j" 'evil-ace-jump-word-mode)
  (define-key evil-motion-state-map ",k" 'evil-ace-jump-char-mode)
  (define-key evil-motion-state-map [up] 'evil-previous-visual-line)
  (define-key evil-insert-state-map [up] 'evil-previous-visual-line)
  (define-key evil-motion-state-map [down] 'evil-next-visual-line)
  (define-key evil-insert-state-map [down] 'evil-next-visual-line)
  (global-set-key [(shift up)] 'windmove-up)
  (global-set-key [(shift down)] 'windmove-down)
  (global-set-key [(shift left)] 'windmove-left)
  (global-set-key [(shift right)] 'windmove-right)
  (global-set-key [f7] 'compile))

(eval-when-compile
  (defmacro init-el-make-mode-line (elements)
    "Make the mode line format list.

ELEMENTS is a list of mode line elements.
Each element can be one of the following:
 * (:string STRING :face FACE) - STRING propertized with FACE. If FACE is nil,
   string is not propertized.
 * (:symbol-name SYMBOL :face FACE) - the name of the SYMBOL propertized with
   FACE.
 * (:status-list BODY) - comma-separated list of strings.
   BODY is a list of elements, each of which should be of the form (:predicate
   PREDICATE :result RESULT :face FACE), where PREDICATE is a boolean expression
   determining whether to display RESULT, RESULT is a string expression to be
   displayed, and FACE is the face to propertize RESULT with."
    (let ((result ())
          (pendingstring nil))
      (dolist (element elements)
        (pcase element
          ((or `(:string ,string :face ,face)
               (guard (when pendingstring
                        ;; Not a string, flush the pending string.
                        (push pendingstring result)
                        (setq pendingstring nil))
                      nil))
           (when face
             (setq string (propertize string 'face face)))
           (if pendingstring
               (setq pendingstring (concat pendingstring string))
             (setq pendingstring string)))
          (`(:symbol-name ,symbol :face ,face)
           (push ``(:eval (,(lambda ()
                              (propertize (symbol-name ,symbol) 'face ,face))))
                 result))
          (`(:status-list ,body)
           (let ((body `(let ((strings ()))
                          ,@(mapcar
                             (lambda (element)
                               (pcase element
                                 (`(:predicate ,predicate :result ,result :face ,face)
                                  (let ((resultexpr `(propertize ,result 'face ,face)))
                                    (when (stringp result)
                                      (setq resultexpr `(eval-when-compile ,resultexpr)))
                                    `(when ,predicate
                                       (push ,resultexpr strings))))
                                 (_
                                  (error "invalid status list element %S" element))))
                             (reverse body))
                          (mapconcat 'identity strings ","))))
             (push ``(:eval (,(lambda () ,body))) result)))
          (_
           (error "unrecognized element: %S" element))))
      (when pendingstring
        (push pendingstring result))
      `(list ,@(nreverse result)))))

(defun init-el-setup-mode-line ()
  (setq-default
   mode-line-format
   (init-el-make-mode-line
    ((:string " " :face nil)
     (:string "%b" :face font-lock-keyword-face)
     (:string " (" :face nil)
     (:string "%02l" :face font-lock-type-face)
     (:string "," :face nil)
     (:string "%02c" :face font-lock-type-face)
     (:string ") [" :face nil)
     (:string "%p" :face font-lock-constant-face)
     (:string "/" :face nil)
     (:string "%I" :face font-lock-constant-face)
     (:string "] [" :face nil)
     (:string "%m" :face font-lock-string-face)
     (:string "] [" :face nil)
     (:symbol-name buffer-file-coding-system :face font-lock-builtin-face)
     (:string "] [" :face nil)
     (:symbol-name evil-state :face font-lock-function-name-face)
     (:string "] %[[" :face nil)
     (:status-list
      ((:predicate (buffer-modified-p)
                   :result "Mod"
                   :face font-lock-warning-face)
       (:predicate buffer-read-only
                   :result "RO"
                   :face font-lock-type-face)
       (:predicate (buffer-narrowed-p)
                   :result "Narrow"
                   :face font-lock-type-face)
       (:predicate defining-kbd-macro
                   :result "Macro"
                   :face font-lock-type-face)))
     (:string "]%]" :face nil)))))

(defun init-el-setup-title-bar ()
  (setq icon-title-format (setq frame-title-format "%b [%f] - Emacs")))

(defun init-el-setup-buffer-boundary-indicators ()
  (setq-default indicate-empty-lines t
                indicate-buffer-boundaries 'left))

(defun init-el-setup-paragraph-filling ()
  (setq sentence-end-double-space nil)
  (setq-default fill-column 80))

(defun init-el-start-server ()
  (when (eq system-type 'windows-nt)
    (server-start)))

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
      (read-only-mode)
      (set-buffer-modified-p nil))
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
        (let* ((bol (line-beginning-position))
               (currentline (buffer-substring-no-properties bol (line-end-position))))
          (if (string-equal previousline currentline)
              (delete-region bol (progn (forward-line 1) (point)))
            (setq previousline currentline)
            (forward-line 1)))))))

(defun create-buffer (name)
  "Create a new buffer with NAME and switch to it.

The buffer starts in `fundamental-mode'."
  (interactive "BBuffer name:")
  (prog1 (switch-to-buffer (generate-new-buffer name))
    (fundamental-mode)))

(defun open-directory-in-external-browser (directory)
  "Open DIRECTORY in the system's default file browser."
  (interactive (let ((bufferfile (buffer-file-name)))
                 (list (if bufferfile
                           (file-name-directory bufferfile)
                         default-directory))))
  (pcase system-type
    (`windows-nt
     (w32-shell-execute "open" directory nil 1))
    (`gnu/linux
     (start-process "" nil "xdg-open" directory))
    (_
     (error "Unknown operating system"))))
