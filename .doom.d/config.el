;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;------ User configuration ------;;;

;; My default user identity as my yt alias
(setq user-full-name "emmet")

;; This shows me normal line numbers
(setq display-line-numbers-type t)

;; Makes for easier editing with wrapped lines
(setq line-move-visual t)

;; Theme
(setq doom-theme 'doom-gruvbox)

;; Transparent background
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
()

;; This makes non-main buffers dimmer, so you can focus on main buffers
(solaire-global-mode +1)

;; Beacon shows where the cursor is, even when fast scrolling
(setq beacon-mode t)

;; Quicker window management keybindings
(bind-key* "C-j" #'evil-window-down)
(bind-key* "C-k" #'evil-window-up)
(bind-key* "C-h" #'evil-window-left)
(bind-key* "C-l" #'evil-window-right)
(bind-key* "C-q" #'evil-window-delete)

;; Disables custom.el
(setq custom-file null-device)

;; Fancy splash image
(setq fancy-splash-image "~/.doom.d/arch.png")

;;;------ Registers ------;;;

(map! :leader
      :desc "Jump to register"
      "r" 'jump-to-register)

(set-register ?f '(file . "/home/emmet/Family.s/Documents/Finances/hledger.journal"))
(set-register ?r '(file . "/home/emmet/README.org"))
(set-register ?d '(file . "/home/emmet/.doom.d/doom.org"))
(set-register ?h '(file . "/home/emmet"))

;;;------ Org mode configuration ------;;;

;; Set default org directory
(setq org-directory "~/.Org")

;; This allows you to actually control how big images are in org docs!
(setq org-image-actual-width nil)

(custom-set-faces!
  '(org-level-1 :inherit outline-1 :height 1.6)
  '(org-level-2 :inherit outline-2 :height 1.4)
  '(org-level-3 :inherit outline-3 :height 1.3)
  '(org-level-4 :inherit outline-4 :height 1.2)
  '(org-level-5 :inherit outline-5 :height 1.15)
  '(org-level-6 :inherit outline-6 :height 1.1)
  '(org-level-7 :inherit outline-7 :height 1.05)
  )

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


;;---- this block from http://fgiasson.com/blog/index.php/2016/06/21/optimal-emacs-settings-for-org-mode-for-literate-programming/ ----;;
;; Tangle Org files when we save them
(defun tangle-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-babel-tangle)))

(add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

;; Enable autorevert globally so that buffers update when files change on disk.
;; Very useful when used with file syncing (i.e. syncthing)
(setq global-auto-revert-mode t)
(setq auto-revert-use-notify nil)

;; ---- end block ---- ;;

;; Custom function to convert org mode to ODP presentation
;; Depends on bash, libreoffice, and pandoc
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

;;;------ Org roam configuration ------;;;

(setq org-roam-directory "~/Roam"
      org-roam-db-location "~/Roam/org-roam.db")

(defun org-roam-switch-db ()
  "Switch to a different org-roam database"
  (interactive)
  (setq full-org-roam-db-list nil)

  (setq full-org-roam-db-list (directory-files "~" t "\\.[p,s]$"))
  (dolist (item full-org-roam-db-list)
    (setq full-org-roam-db-list
          (append (directory-files item t "\\.[p,s]$") full-org-roam-db-list)))

  (setq full-org-roam-db-list-pretty (list "Default"))
  (dolist (item full-org-roam-db-list)
    (setq full-org-roam-db-list-pretty
          (append (list
                   (replace-regexp-in-string "\\/home\\/emmet\\/" "" item)) full-org-roam-db-list-pretty)))

  (setq org-roam-db-choice (completing-read "Select org roam database: "
                            full-org-roam-db-list-pretty nil t))
  (if (string= org-roam-db-choice "Default")
      (setq org-roam-directory "~/Roam"
            org-roam-db-location "~/Roam/org-roam.db")
      (setq org-roam-directory (concat "~/" org-roam-db-choice "/Roam")
            org-roam-db-location (concat "~/" org-roam-db-choice "/Roam/org-roam.db")))
  (if (string= org-roam-db-choice "Default")
      (dired "~/Roam")
      (dired (concat "~/" org-roam-db-choice "/Roam")))

  (message (concat "Switched to " org-roam-db-choice " org-roam database!")))

(map! :leader
      :prefix ("N" . "org-roam notes")
      :desc "Capture new roam node"
      "c" 'org-roam-capture)

(map! :leader
      :prefix ("N" . "org-roam notes")
      :desc "Insert roam node link at point"
      "i" 'org-roam-node-insert)

(map! :leader
      :prefix ("N" . "org-roam notes")
      :desc "Find roam node"
      "." 'org-roam-node-find)

(map! :leader
      :prefix ("N" . "org-roam notes")
      :desc "Switch org-roam database"
      "s" 'org-roam-switch-db)

(org-roam-db-autosync-mode)

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
  (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "${title}\n")
  :unnarrowed t))))

;;;------ Org agenda configuration ------;;;

;; Set folder for my org agenda files
(setq org-agenda-files (list "/home/emmet/Family.s/Agenda"
                             "/home/emmet/Producer.p/Agenda"
                             "/home/emmet/Agenda"
                             "/home/emmet/Teaching.p/Agenda"
                             "/home/emmet/Author.p/Agenda"
                             "/home/emmet/Gamedev.p/Agenda"))

;; Function to be run when org-agenda is opened
(defun org-agenda-open-hook ()
  "Hook to be run when org-agenda is opened"
  )

;; Adds hook to org agenda mode, making follow mode active in org agenda
(add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)

;; Function to list all my available org agenda files and switch to them
(defun list-and-switch-to-agenda-file ()
  "Lists all available agenda files and switches to desired one"
  (interactive)
  (setq full-agenda-file-list nil)
  (dolist (item org-agenda-files)
    (setq full-agenda-file-list (append (directory-files item t org-agenda-file-regexp) full-agenda-file-list)))
  (setq choice (completing-read "Select agenda file:" full-agenda-file-list nil t))
  (find-file choice))

(map! :leader
      :desc "Switch to specific org agenda file"
      "o a s" 'list-and-switch-to-agenda-file)

(map! :leader
      :desc "Open org calendar"
      "o c" #'cfw:open-org-calendar)

;;;------ magit configuration ------;;;

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

;;;------ elfeed configuration ------;;;

(map! :leader
      :desc "Open elfeed"
      "o n" #'elfeed)

;;;------ mu4e configuration ------;;;

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

;;;-- hledger-mode configuration ;;;--

;;; Basic configuration
(require 'hledger-mode)

;; To open files with .journal extension in hledger-mode
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

;; The default journal location is too opinionated.
(setq hledger-jfile "/home/emmet/Family.s/Documents/Finances/hledger.journal")

;;; Auto-completion for account names
;; For company-mode users:
(add-to-list 'company-backends 'hledger-company)

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
      :desc "Exec hledger command"
      "d" 'hledger-daily-report*)

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

;;;------ Load my private config ------;;;

(load! "~/.doom.d/private.el")

;;;------ Extra ------;;;

;; This line is here so that my org calendar works properly
;; Auto opens org agenda on server startup
(org-agenda-list)
