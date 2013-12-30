(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;
;;; UI
;;;

(load-theme 'zenburn t)
(set-default-font "DejaVu Sans Mono 10")
(menu-bar-mode t)

(ido-vertical-mode 1)

(require 'projectile)
(projectile-global-mode)

;;;
;;; Magit
;;;

(require 'magit)

;; full screen magit-status
;; http://whattheemacsd.com/setup-magit.el-01.html

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;;
;;; Erlang Mode.
;;;
(setq auto-mode-alist
   (cons '("\\.erl" . erlang-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.hrl" . erlang-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.app" . erlang-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.rel" . erlang-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.appup" . erlang-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.yaws" . erlang-mode) auto-mode-alist))


;;; WINDOW SPLITING
(global-set-key (kbd "<f1>") 'delete-other-windows)
(global-set-key (kbd "<f2>") 'split-window-vertically)
(global-set-key (kbd "<f5>") 'other-window)



;;;                                                                                        
;;; windows navigation                                                                     
;;;                                                                                        
                                                                                           
(global-set-key (kbd "M-<left>") 'windmove-left)          ; move to left windnow         
(global-set-key (kbd "M-<right>") 'windmove-right)        ; move to right window         
(global-set-key (kbd "M-<up>") 'windmove-up)              ; move to upper window         
(global-set-key (kbd "M-<down>") 'windmove-down)          ; move to downer window        

(global-set-key (kbd "M-<next>") 'enlarge-window)
(global-set-key (kbd "M-<prior>") 'shrink-window)



;;;
;;; imenu
;;; http://www.masteringemacs.org/articles/2011/01/14/effective-editing-movement/
;;;

(defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))

;(global-set-key "\C-ci" 'ido-goto-symbol) ; or any key you see fit
(global-set-key (kbd "M-i") 'ido-goto-symbol)


;;;
;;; save all files
;;;
(global-set-key (kbd "\C-x\C-s") '(lambda () (interactive) (save-some-buffers t)))
 

;;;
;;; javascript mode
;;;

(add-to-list 'auto-mode-alist '("\\.js$"  . js2-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . html-mode))
(setq js2-basic-offset 2)


;;;
;;; erlang mode
;;;

(add-hook 'erlang-mode-hook (lambda () (linum-mode 1)))
(add-hook 'erlang-mode-hook (lambda() (setq indent-tabs-mode nil)))

;; export automatique


(defun stripwhite (str)
  "Remove any whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "[ \t\n]*" "" s)))

(defun erlang-move-to-export-insertion ()
  (interactive)
    (goto-char (point-max))
    (if (search-backward-regexp "^-export" 0 t)
	(end-of-line)
      (search-backward-regexp "^-" 0 t)
      (end-of-line)))

(defun erlang-export (fun-arity)
  (interactive
   (list (read-no-blanks-input "function/arity: " (erlang-current-function))))
  (save-excursion
    (erlang-move-to-export-insertion)
    (newline)
    (insert (format "-export ([%s])." fun-arity))))

(defun erlang-current-function ()
  (save-excursion
    (if (not (equal (point) (point-max))) (forward-char))
    (erlang-beginning-of-function)
    (let ((beg (point))
	  (fun-name)
	  (fun-arity)
	  (result '()))
      (search-forward "(")
      (backward-char)
      (set 'fun-name (stripwhite (buffer-substring beg (point))))
      (if (char-equal (char-after) ?\))
	  (set 'fun-arity 0)
	(forward-char)
	(set 'fun-arity 0)
	(while (not (char-equal (char-after) ?\)))
	  (erlang-forward-arg)
	  (set 'fun-arity (+ fun-arity 1))))
      (format "%s/%d" fun-name fun-arity))))

(defun erlang-forward-arg ()
  (forward-sexp))


(add-hook 'erlang-mode-hook (lambda () (local-set-key "\C-ce" 'erlang-export)))

