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
 '(rainbow-delimiters-depth-1-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "magenta")))))

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
      
;; FIXME está inserindo uma linha no final desnecessariamente.
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
;; (add-to-list 'default-frame-alist '(font . "IBM Plex Mono-11" ))
;; (set-face-attribute 'default t :font "IBM Plex Mono-10" )

(add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-12" ))
(set-face-attribute 'default t :font "Fantasque Sans Mono-12" )

;; (add-to-list 'default-frame-alist '(font . "Courier Prime-10" ))
;; (set-face-attribute 'default t :font "Courier Prime-10" )

;; (add-to-list 'default-frame-alist '(font . "Monoid-11" ))
;; (set-face-attribute 'default t :font "Monoid-11" )

;; Save clipboard.
(setq-default save-interprogram-paste-before-kill t)
(setq save-interprogram-paste-before-kill t)
(setq-default yank-pop-change-selection t)

;; Highlight comment (labels).
(add-to-list 'load-path "~/emacs_ext/hl-todo")
(require 'hl-todo)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FC0303")
        ("FIXME"  . "#F4FC03")
        ("DEBUG"  . "#DF6722")
        ("WARNING"  . "#F800FC")
        ("NOTE"   . "#0011FC")
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

;; Wrap selected with ", (, [, {
(global-set-key (kbd "M-C-{") 'insert-pair)
(global-set-key (kbd "M-C-(") 'insert-pair)
(global-set-key (kbd "M-C-[") 'insert-pair)
(global-set-key (kbd "C-\"") 'insert-pair)

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
 '(org-agenda-files '("/home/matheusc/Documents/codes/org-mode/1.org"))
 '(package-selected-packages
   '(telephone-line docker-compose-mode dockerfile-mode ess bnf-mode sphinx-doc cmake-project cmake-font-lock sml-mode crux multiple-cursors rainbow-delimiters cyberpunk-theme dracula-theme ##))
 '(warning-suppress-types '((emacs))))
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
;;(load-theme 'dracula t)
;;(load-theme 'cyberpunk t)

;; Delete selected region.
(delete-selection-mode 1)

;; enable centered window mode.
(add-to-list 'load-path "~/emacs_ext/")
(require 'centered-window)
;;(centered-window-mode t) <-- disabled

;; https://github.com/Malabarba/beacon
;; (add-to-list 'load-path "~/emacs_ext/beacon/")
;; (require 'beacon)
;; (setq beacon-size 40)
;; (setq beacon-color "green")
;; (beacon-mode 1)


;; https://github.com/nschum/highlight-symbol.el
(add-to-list 'load-path "~/emacs_ext/highlight-symbol.el/")
(require 'highlight-symbol)

(global-set-key (kbd "C-c C-h") 'highlight-symbol)
(global-set-key (kbd "C-c C-n") 'highlight-symbol-next)
(global-set-key (kbd "C-c C-p") 'highlight-symbol-prev)
(global-set-key (kbd "C-c C-r") 'highlight-symbol-query-replace)


;; https://github.com/gonewest818/dimmer.el
;; (add-to-list 'load-path "~/emacs_ext/dimmer.el/")
;; (require 'dimmer)
;; (dimmer-configure-which-key)
;; (dimmer-configure-helm)
;; (setq dimmer-fraction 0.4)
;; (dimmer-mode t)

;; https://github.com/emacsmirror/rainbow-mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-<") 'mc/mark-all-like-this)

(add-to-list 'load-path "~/emacs_ext/projectile/")
(require 'projectile)
(projectile-mode +1)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; https://github.com/jaypei/emacs-neotree
(add-to-list 'load-path "~/emacs_ext/emacs-neotree/")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; https://github.com/domtronn/all-the-icons.el
(add-to-list 'load-path "~/emacs_ext/all-the-icons.el/")
(require 'all-the-icons)
(all-the-icons-octicon "file-binary")  ;; GitHub Octicon for Binary File
(all-the-icons-faicon  "cogs")         ;; FontAwesome icon for cogs
(all-the-icons-wicon   "tornado")      ;; Weather Icon for tornado

;; save last cursor position
(save-place-mode 1) 

;; Enable ORG mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Disable menu-bar, tool-bar and scroll-bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar 0)

;; Git conflit 
(setq smerge-command-prefix "\C-cv")

;; Stages of Org mode
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "TESTING" "DONE")))

;; Python snippets for docstring
 (add-hook 'python-mode-hook (lambda ()
                               (require 'sphinx-doc)
                               (sphinx-doc-mode t)))


;; https://github.com/dbordak/telephone-line
(require 'telephone-line)
(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))

(telephone-line-mode 1)
