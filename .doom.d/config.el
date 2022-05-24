;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; My default user identity as my yt alias
(setq user-full-name "emmet")

(load! "~/.doom.d/private.el")

;; This shows me normal line numbers
(setq display-line-numbers-type t)

;; Theme
(setq doom-theme 'doom-oceanic-next)

;; Beacon shows where the cursor is, even when fast scrolling
(setq beacon-mode t)

;; This makes non-main buffers dimmer, so you can focus on main buffers
(solaire-global-mode +1)

;; This allows you to actually control how big images are in org docs!
(setq org-image-actual-width nil)

;; Pretty org bullets
(use-package org-bullets
    :ensure t
        :init
        (add-hook 'org-mode-hook (lambda ()
                                   (org-bullets-mode 1))))

; Automatic table of contents is nice
(if (require 'toc-org nil t)
    (progn
      (add-hook 'org-mode-hook 'toc-org-mode)

      ;; enable in markdown, too
      (add-hook 'markdown-mode-hook 'toc-org-mode))
  (warn "toc-org not found"))

;; Set folder for my org agenda files
(setq org-agenda-files (list "/home/emmet/Family.s/Agenda"
                             "/home/emmet/Producer.p/Agenda"
                             "/home/emmet/Agenda"
                             "/home/emmet/Teaching.p/Agenda"
                             "/home/emmet/Gamedev.p/Agenda"))

;;---- this block from http://fgiasson.com/blog/index.php/2016/06/21/optimal-emacs-settings-for-org-mode-for-literate-programming/ ----;;
;; Tangle Org files when we save them
(defun tangle-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-babel-tangle)))

(add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

;; Enable the auto-revert mode globally. This is quite useful when you have
;; multiple buffers opened that Org-mode can update after tangling.
;; All the buffers will be updated with what changed on the disk.
(global-auto-revert-mode)

;; Add Org files to the agenda when we save them
(defun to-agenda-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-agenda-file-to-front)))

(add-hook 'after-save-hook 'to-agenda-on-save-org-mode-file)
;; ---- end block ---- ;;

;; Need the following two blocks to make magit work with git bare repos
(defun ~/magit-process-environment (env)
  "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory.
https://github.com/magit/magit/issues/460 (@cpitclaudel)."
  (let ((default (file-name-as-directory (expand-file-name default-directory)))
        (home (expand-file-name "~/")))
    (when (string= default home)
      (let ((gitdir (expand-file-name "~/.dotfiles.git/")))
        (push (format "GIT_WORK_TREE=%s" home) env)
        (push (format "GIT_DIR=%s" gitdir) env))))
  env)

(advice-add 'magit-process-environment
            :filter-return #'~/magit-process-environment)

(map! :leader
      :desc "Open elfeed"
      "o n" #'elfeed)

(map! :leader
      :desc "Open org calendar"
      "o c" #'cfw:open-org-calendar)

(bind-key* "C-j" #'evil-window-down)
(bind-key* "C-k" #'evil-window-up)
(bind-key* "C-h" #'evil-window-left)
(bind-key* "C-l" #'evil-window-right)
(bind-key* "C-q" #'evil-window-delete)

;; Auto-load mu4e and org-mu4e on start
(require 'mu4e)

;; mu4e update command
(setq mu4e-get-mail-command "mbsync -a")

;; Run mu4e update every n seconds
(setq mu4e-update-interval 100)

;; Set maildir for mu4e
(setq mu4e-root-maildir "~/.mail")

;; Set important folders for mu4e
(setq mu4e-sent-folder     "/Sent"
      mu4e-drafts-folder   "/Drafts"
      mu4e-trash-folder    "/Trash")

;; My mu4e headers
(setq mu4e-headers-fields
      '((:from            . 22)
        (:human-date      . 12)
        (:flags           .  6)
        (:maildir         . 30)
        (:thread-subject  . nil)))

;; Following excerpt improves deleting in mu4e

;; Excerpt taken from Gregory J Stein
;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
(defun remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))
(setq mu4e-marks (remove-nth-element 5 mu4e-marks))
(add-to-list 'mu4e-marks
     '(trash
       :char ("d" . "▼")
       :prompt "dtrash"
       :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
       :action (lambda (docid msg target)
                 (mu4e~proc-move docid
                    (mu4e~mark-check-target target) "-N"))))

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(setq custom-file null-device)

(defun org-agenda-open-hook ()
  "Hook to be run when org-agenda is opened"
  (org-agenda-follow-mode)
  )

(add-hook 'org-agenda 'org-agenda-open-hook)

(defun my-ox-odp ()
  "Convert an org mode file to an ODP presentation."
  (interactive)
  (setq file-name (buffer-file-name))
  (setq output-pptx-file-name (replace-regexp-in-string "\.org" "\.pptx" (buffer-file-name)))
  (setq output-odp-file-name (replace-regexp-in-string "\.org" "\.odp" (buffer-file-name)))
  (setq odp-style-file-name (completing-read "Choose style: "
                                             '(("/home/emmet/.doom.d/scripts/ox-odp/styles/water.odp")) nil t))
  (shell-command (concat "~/.doom.d/scripts/ox-odp.sh \"" (buffer-file-name) "\" \"" odp-style-file-name "\" > /dev/null"))
  )

(map! :leader
      :desc "Convert org document to odp presentation"
      "e p" 'my-ox-odp)

;;;-- hledger-mode configuration ;;;--
;;; Basic configuration
(require 'hledger-mode)

;; To open files with .journal extension in hledger-mode
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

;; Provide the path to you journal file.
;; The default location is too opinionated.
(setq hledger-jfile "/home/emmet/Family.s/Documents/Finances/hledger.journal")


;;; Auto-completion for account names
;; For company-mode users,
(add-to-list 'company-backends 'hledger-company)

(map! :leader
      :desc "Jump to register"
      "r" 'jump-to-register)

(set-register ?f '(file . "/home/emmet/Family.s/Documents/Finances/hledger.journal"))
(set-register ?r '(file . "/home/emmet/README.org"))

(map! :leader
      :prefix ("l" . "hledger")
      :desc "Exec hledger command"
      "c" 'hledger-run-command)

(map! :leader
      :prefix ("l" . "hledger")
      :desc "Generate hledger balancesheet"
      "b" 'hledger-balancesheet*)

(map! :leader
      :prefix ("l" . "hledger")
      :desc "Add new entry to hledger journal"
      "e" 'hledger-jentry)

(map! :localleader
      :map hledger-mode-map
      :desc "Reschedule transaction at point"
      "d s" 'hledger-reschedule)

(map! :localleader
      :map hledger-mode-map
      :desc "Edit amount at point"
      "t a" 'hledger-edit-amount)

(org-agenda-list)
