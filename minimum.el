;;; minimum.el --- minimum settings for emacs init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  full name

;; Author: Oishi Kento oishi.kento@gmail.com
;; Keywords: init.el

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
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq-default tab-width 4 indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(global-linum-mode t)
(setq linum-format"%4d ")
(global-set-key [f6] 'linum-mode)
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(setq ring-bell-function 'ignore)
(setq frame-title-format "%f")
(setq initial-scratch-message "")



(provide 'minimum)
;;; minimum.el ends here
