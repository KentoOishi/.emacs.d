;; package.el
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; (setq my-settings "~/.emacs.d/my_settings.el")
;; (load my-settings t)

;; カラーテーマ
(load-theme 'wombat t)

;; 日本語 UTF-8
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; スタート画面のメッセージを消す
(setq inhibit-startup-message t)

;; バックアップファイルを作らない
(setq make-backup-files nil)

;; 終了時オートセーブファイル削除
(setq delete-auto-save-files t)

;; tab -> space*4
(setq-default tab-width 4 indent-tabs-mode nil)

;; 改行コードの表示
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; ウィンドウの透明化
;; active / not-active (= alpha)
(add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))

;; メニューバーを消す
(menu-bar-mode -1)

;; ツールバーを消す
(tool-bar-mode -1)

;; 列数の表示
(column-number-mode t)

;; 行数の表示
;; 4桁分の領域を確保
;; [f6]で行数表示の切り替え
(global-linum-mode t)
(setq linum-format"%4d ")
(global-set-key [f6] 'linum-mode)

;; 対応する()を光らせる
(show-paren-mode 1)

;; space tab の可視化
;;(global-whitespace-mode 1)

;; 複数ウィンドウを禁止する
(setq ns-pop-up-frames nil)


;; スクロール行数
(setq scroll-conservatively 1)
(setq scroll-margin 10)
(setq next-screen-context-lines 10)
(setq scroll-preserve-screen-position t)

;; yes, no -> y, n
(fset 'yes-or-no-p 'y-or-n-p)

;; buffer
(global-set-key "\C-x\C-b" 'bs-show)

;; trr
(add-to-list 'load-path "~/.emacs.d/emacs-trr")
(require 'trr)

;; window move shift+矢印
(windmove-default-keybindings)

;; ピープ音
(setq ring-bell-function 'ignore)


;; mozc
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)

;; 非アクティブウィンドウの背景色の変更
;(require 'hiwin)
;(hiwin-activate)
;(set-face-background 'hiwin-face "gray30")

;; 現在ポインタのある関数をモードラインに表示
(which-function-mode 1)

;; リージョンのハイライト
(transient-mark-mode 1)

;; タイトルにフルパス
(setq frame-title-format "%f")

;; current directry の表示
(let ((ls (member 'mode-line-buffer-identification
                  mode-line-format)))
  (setcdr ls
          (cons '(:eval (concat " ("
                                (abbreviate-file-name default-directory)
                                ")"))
                (cdr ls))))

;; auto complete
(require 'auto-complete-config)
;;(ac-config-default)
(global-auto-complete-mode)
;; elscreen (tab)
;;(require 'elscreen)
;;(elscreen-start)

;; neo tree (side bar)
(require 'neotree)
(global-set-key [f5] 'neotree-toggle)

;; golden ratio
;(golden-ratio-mode 1)
;(add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")

; ----------------------- YaTeX ------------------------------
;(setq auto-mode-alist
;   (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
;(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "platex")
(setq bibtex-command "pbibtex")
;(setq dvi2-command "open -a Preview")
;(setq YaTeX-prefix "\C-t")
;(defvar YaTeX-dvi2-command-ext-alist
; '(("Preview\\|Skim" . ".pdf")
;   ("gv" . ".ps")
;   ("pxdvi\\|xdvi\\|dvipdfmx" . ".dvi")
;  )
; )
;(setq latex-run-command "\C-c \C-c")
;;(setq tex-pdfview-command "open -a Preview");
;
;(setq YaTeX-kanji-code 4);;

;(provide 'yatex-startup)
;
;(add-hook 'yatex-mode-hook '(lambda ()
;                              (electric-indent-local-mode -1)))
;-------------------------------------------------------------


;; web-mode for html, js etc
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
'(("php"    . "\\.phtml\\'")
  ("blade"  . "\\.blade\\.")))
(defun web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-engines-alist
        '(("php"    . "\\.ctp\\'"))
        )
  )
(add-hook 'web-mode-hook  'web-mode-hook)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-paring t)
(setq web-mode-ac-sources-alist
  '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
    ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
    ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))


;; copy and paste from clipboard
(defun my-cut-function (text &optional rest)
  (interactive)
  (let ((process-connection-type nil))
    (let ((proc (start-process "xclip" "*Messages*" "xclip")))
      (process-send-string proc text)
      (process-send-eof proc))))
(defun my-paste-function ()
  (interactive)
  (shell-command-to-string "xclip -o"))
(when (and (not window-system)
         (executable-find "xclip"))
  (setq interprogram-cut-function 'my-cut-function)
  (setq interprogram-paste-function 'my-paste-function))

;; 括弧を閉じる
(electric-pair-mode 1)

;; C-kで行全体を削除する
;;(setq kill-whole-line t)

;; C-h -> backspace
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key key-translation-map (kbd "M-h") (kbd "<C-backspace>"))
;;(define-key key-translation-map (kbd "DEL") (kbd "C-h"))
;;(define-key key-translation-map (kbd "M-DEL") (kbd "M-h"))

;; 空白を一度に削除
(if (fboundp 'global-hungry-delete-mode)
    (global-hungry-delete-mode 1))

;; png, jpg などを表示
(setq auto-image-file-mode t)


;; mで1左, oで1右
;;(define-key key-translation-map (kbd "C-m") (kbd "C-f"))
;;(define-key key-translation-map (kbd "C-o") (kbd "C-b"))

;; maxima, imaxima for mathematica
(autoload 'imaxima "imaxima" "Frontend of Maxima CAS" t)
(autoload 'imath "imath" "Interactive Math mode" t)
(autoload 'imath-mode "imath" "Interactive Math mode" t)
;;(setq imaxima-fnt-size "large")

;; fomt-size
;;(add-to-list 'default-frame-alist
;;             '(font . "DejaVu Sans Mono-16"))

;;php
(unless (package-installed-p 'ac-php )
  (package-refresh-contents)
  (package-install 'ac-php )
  )
(require 'cl)
(require 'php-mode)
(add-hook 'php-mode-hook
          '(lambda ()
             (auto-complete-mode t)
             (require 'ac-php)
             (setq ac-sources  '(ac-source-php ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers) )
             (yas-global-mode 1)
             (ac-php-core-eldoc-setup ) ;; enable eldoc

             (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
             (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back)    ;go back
             ))

(require 'auto-complete-clang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 雛形の作成 ;;
;;;;;;;;;;;;;;;;
(require 'autoinsert)
(setq user-id-string "id")
(setq user-full-name "full name")
(setq user-mail-address "mail address")

;; テンプレートのディレクトリ
(setq auto-insert-directory "~/.emacs.d/template")

;; 各ファイルによってテンプレートを切り替える
(setq auto-insert-alist
      (nconc '(
               ("\\.py$" . ["template.py" my-template])
               ) auto-insert-alist))
(require 'cl)

(defvar template-replacements-alists
  '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ;; ("%date%" . (lambda () (current-time-string)))
    ("%date%" . (lambda () (format-time-string "%Y-%m-%d %H:%M:%S")))
    ("%mail%" . (lambda () (identity user-mail-address)))
    ("%name%" . (lambda () (identity user-full-name)))
    ("%id%" . (lambda () (identity user-id-string)))
    ))

(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq yatex-settings "~/.emacs.d/tex.el")
(load yatex-settings)

(setq vrml-settings "~/.emacs.d/packages/vrml-mode.el")
(load vrml-settings)

(setq language-settings "~/.emacs.d/language-settings.el")
(load language-settings)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auto-complete-clang-async auto-complete-clang company-irony irony ac-php php-completion php-mode jedi ac-python ac-html ac-js2python iedit web-mode-edit-element web-mode yatex neotree elscreen auto-complete hiwin mozc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; *scratch* の初期メッセージ
(setq initial-scratch-message "")


