;;
;; html
;;
(require 'ac-html)

;;
;; python
;;
;;(require 'ac-python)
;;(add-to-list 'ac-modes 'python-2-mode)
;;(add-to-list 'ac-modes 'python-3-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
