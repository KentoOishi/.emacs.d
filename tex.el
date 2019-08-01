;;; tex.el --- tex setting                           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  full name

;; Author: full name <mail address>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:



;;
;; YaTeX
;;
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvi2-command-ext-alist
      '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|atril\\|xreader\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|MicrosoftEdge\\|microsoft-edge\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
(setq tex-command "platex")
;(setq tex-command "lualatex -synctex=1")
;(setq tex-command "latexmk")
;(setq tex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;(setq tex-command "latexmk -e '$lualatex=q/lualatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdflua")
(setq bibtex-command "platexmk")
(setq makeindex-command  "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;(setq dvi2-command "xdg-open")
(setq dvi2-command "evince")
;(setq dvi2-command "atril")
;(setq dvi2-command "okular --unique")
;(setq dvi2-command "zathura -x \"emacsclient --no-wait +%{line} %{input}\"")
;(setq dvi2-command "qpdfview --unique")
;(setq dvi2-command "texworks")
;(setq dvi2-command "texstudio --pdf-viewer-only")
;(setq tex-pdfview-command "xdg-open")
(setq tex-pdfview-command "evince")
;(setq tex-pdfview-command "atril")
;(setq tex-pdfview-command "okular --unique")
;(setq tex-pdfview-command "zathura -x \"emacsclient --no-wait +%{line} %{input}\"")
;(setq tex-pdfview-command "qpdfview --unique")
;(setq tex-pdfview-command "texworks")
;(setq tex-pdfview-command "texstudio --pdf-viewer-only")
(setq dviprint-command-format "wine cmd /c start AcroRd32.exe `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")
(require 'dbus)
(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))
(defun evince-inverse-search (file linecol &rest ignored)
  (let* ((fname (decode-coding-string (url-unhex-string (un-urlify file)) 'utf-8))
         (buf (find-file fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col))
      (x-focus-frame (selected-frame)))))
(dbus-register-signal
 :session nil "/org/gnome/evince/Window/0"
 "org.gnome.evince.Window" "SyncSource"
 'evince-inverse-search)
(with-eval-after-load 'yatexprc
  (defun YaTeX-preview-jump-line ()
    "Call jump-line function of various previewer on current main file"
    (interactive)
    (save-excursion
      (save-restriction
        (widen)
        (let*((pf (or YaTeX-parent-file
                      (save-excursion (YaTeX-visit-main t) (buffer-file-name))))
              (pdir (file-name-directory pf))
              (bnr (substring pf 0 (string-match "\\....$" pf)))
              ;(cf (file-relative-name (buffer-file-name) pdir))
              (cf (buffer-file-name)) ;2016-01-08
              (buffer (get-buffer-create " *preview-jump-line*"))
              (line (count-lines (point-min) (point-end-of-line)))
              (previewer (YaTeX-preview-default-previewer))
              (cmd (cond
                    ((string-match "Skim" previewer)
                     (format "%s %d '%s.pdf' '%s'"
                             YaTeX-cmd-displayline line bnr cf))
                    ((string-match "evince" previewer)
                     (format "%s '%s.pdf' %d '%s'"
                             "fwdevince" bnr line cf))
                    ((string-match "sumatra" previewer)
                     (format "%s \"%s.pdf\" -forward-search \"%s\" %d"
                             previewer bnr cf line))
                    ((string-match "zathura" previewer)
                     (format "%s --synctex-forward '%d:0:%s' '%s.pdf'"
                             previewer line cf bnr))
                    ((string-match "qpdfview" previewer)
                     (format "%s '%s.pdf#src:%s:%d:0'"
                             previewer bnr cf line))
                    ((string-match "okular" previewer)
                     (format "%s '%s.pdf#src:%d %s'"
                             previewer bnr line (expand-file-name cf)))
                    )))
          (YaTeX-system cmd "jump-line" 'noask pdir))))))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))
;;
;; RefTeX with YaTeX
;;
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))

;;; tex.el ends here
