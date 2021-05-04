;; Matheus' Emacs init file.
;; Edited since 26/06/2020

;; Replace <TAB> to <SPC>
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent nil)

(require 'iso-transl)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Set line numbering.
(setq-default display-line-numbers t)
(setq-default display-line-numbers-type t)

;; Hide the welcome screen in emacs.
(setq inhibit-startup-screen nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)0
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(set-language-environment "Latin-1")

;; eThe below line cause emacs to add two spaces after a colon.
(setq-default colon-double-space t)

;; Set cursor color
(set-cursor-color "green")

;; Set mouse color
(set-mouse-color "white")

;; Set foreground and background
(set-foreground-color "white")
(set-background-color "black")

;;; Set highlighting color for isearch and drag
(set-face-foreground 'region "black")
(set-face-background 'region "white")

;; Change cursor type to I-bean
(setq-default cursor-type 'bar)

;;; Copy current line above (\C-cp) or below (\C-cn).
(defun copy-current-line (above_below &optional N)
  "Copy the line where point is N times above/below it depending of ABOVE_BELOW."
  (interactive "p")
  (progn
    (unless N (setq N 1))
    (beginning-of-line)
    (push-mark)
    (end-of-line)
    (copy-region-as-kill (mark) (point))
    (while (> N 0)
      (if above_below (progn (forward-line -1)(end-of-line)))
       (insert "\n")
      (yank)
      (setq N (1- N)))
    (setq kill-ring (cdr kill-ring))))

(defun copy-current-line-above (&optional N)
  "Copy the line where point is above."
  (interactive "p")
  (copy-current-line t N))

(defun copy-current-line-below (&optional N)
  "Copy the line where point is below."
  (interactive "p")
  (copy-current-line nil N))

(defun kill-current-line ()
  "Delete line where point is."
  (interactive)
  (beginning-of-line)
  (kill-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom MaC-pattern-enabled t "Activates the pattern when evaluating expr.")
(defcustom MaC-pattern "{{\\(.*?\\)}}" "The pattern.")
(defcustom MaC-override nil "Override lines below.")

(defun count-ocurrences (pattern string)
  "count how many times pattern occur in string"
  (let ((n 0))
    (save-excursion
      (save-current-buffer
        (get-buffer-create "temp-buffer")
        (set-buffer "temp-buffer")
        (insert string)
        (goto-char 0)
        (setq n (count-matches pattern))
        (kill-buffer "temp-buffer")))
    n))      

(defun eval-string (pattern string)
  "evaluates expressions that match with pattern in string and replace it."
  (let ((last 0)
        (expr nil)
        (value 0))
    (while (string-match pattern string)
      (setq expr (match-string 1 string))
      (setq expr (eval (car (read-from-string (format "(progn (%s))" expr)))))
      (setq string
            (concat (substring string 0 (match-beginning 0))
                    (number-to-string expr)
                    (substring string (match-end 0)))))
    string))

(eval-string "{{\\(.*?\\)}}" "asdfsdf * 4 2")
      
;; FIXME está inserindo uma linha no final desnecessáriamente.
;; insert a sequence of number one below other.
(defun insert-seq (pattern from to step)
  "insert a sequence in each line in the same column"
  (interactive
   (list
    (setq pattern (read-from-minibuffer "Pattern: "))
    (setq from (string-to-number (read-from-minibuffer "From: ")))
    (setq to (string-to-number (read-from-minibuffer "To: ")))
    (setq step (string-to-number (read-from-minibuffer "Step: ")))))
  (save-excursion
    (let ((n from)
          (column (current-column))
          (expr nil))
      (while (<= n to)
        (setq expr
              (apply 'format
                     (cons pattern
                           (make-list
                            (count-ocurrences "%d" pattern)
                            n))))
        (if MaC-pattern-enabled 
            (insert (eval-string MaC-pattern expr))
          (insert expr))
        (setq n (+ n step))
        (when (or (not MaC-override) (> (forward-line 1) 0))
            (insert ?\n))
        (move-to-column column)
        (while (< (current-column) column)
          (insert ?\s))
        )
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-cp" 'copy-current-line-above)
(global-set-key "\C-cn" 'copy-current-line-below)
(global-set-key "\C-ck" 'kill-current-line)

;; Use UTF-8.
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "pt_BR.UTF-8")
(prefer-coding-system 'utf-8)
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding

;; ruler at column 80.
(setq-default fill-column 80)
(setq-default display-fill-column-indicator-character ?|)
(global-display-fill-column-indicator-mode)

;; number of character in line number indicator.
(setq-default display-line-numbers-width 4)

;;
(setq-default scroll-preserve-screen-position 5)

;; Change default font size.
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-10" ))
(set-face-attribute 'default t :font "IBM Plex Mono-10" )

;; Save clipboard.
(setq-default save-interprogram-paste-before-kill t)
(setq save-interprogram-paste-before-kill t)
(setq-default yank-pop-change-selection t)

;; Highlight comment (labels).
(add-to-list 'load-path "~/emacs_ext/hl-todo")
(require 'hl-todo)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#0000FF")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#DF6722")
        ("WARNING"  . "#FFFF00")
        ("NOTE"   . "#4B0082")
        ("HYPOTHESIS" . "#00FF00")))
(global-hl-todo-mode)

;; Move to begin of indentation -> begin of line.
;; TODO check is there are only white spaces between begin on indentation and
;; beginning of line. If it's true, first move to begin of indentation ans so
;; to begin of line.
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key "\C-a" 'beginning-of-line-or-indentation)


;; Disable welcome screen.
(setq inhibit-startup-screen t)

;; Melpa package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(cyberpunk-theme dracula-theme ##)))
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
;;(load-theme 'dracula t)
(load-theme 'cyberpunk t)

;; Delete selected region.
(delete-selection-mode 1)

;; enable centered window mode.
(add-to-list 'load-path "~/emacs_ext/")
(require 'centered-window)
(centered-window-mode t)

