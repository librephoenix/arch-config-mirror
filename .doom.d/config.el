;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;------ User configuration ------;;;

;; My default user identity as my yt alias
(setq user-full-name "emmet")

;; This shows me normal line numbers
(setq display-line-numbers-type 'visual)

;; Makes for easier editing with wrapped lines
(setq line-move-visual t)

;; Theme
(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "Inconsolata" :size 20))

;; Transparent background
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

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

;; Requires for faster loading
(require 'org-agenda)
(require 'dired)

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

(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(setq org-download-screenshot-method "flameshot gui -p %s")
(after! org-download
   (setq org-download-method 'directory))

(after! org
  (setq-default org-download-image-dir "img/"
        org-download-heading-lvl nil))

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (shell-command (concat "emacs-wayshot " filename))
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(defun my-org-paste()
  "Take an image from the clipboard into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory (buffer-file-name))
                  "img/"
                  (file-name-nondirectory (buffer-file-name))
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (shell-command (concat "wl-paste > " filename))
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(defun my-org-new-file-from-template()
  "Copy a template from ~/Templates into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq template-file (completing-read "Template file:" (directory-files "~/Templates")))
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory (buffer-file-name))
                  "files/"
                  (file-name-nondirectory (buffer-file-name))
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) (file-name-extension template-file t)))
  (copy-file (concat "/home/emmet/Templates/" template-file) filename)
  (setq prettyname (read-from-minibuffer "Pretty name:"))
  (insert (concat "[[./files/" (file-name-nondirectory filename) "][" prettyname "]]"))
  (org-display-inline-images))

(defun my-better-link-opener()
  "Open a link with mimeo instead of using emacs"
  (interactive)
  (setq the-link (expand-file-name (link-hint-copy-link-at-point)))
  (setq the-command (if (string= (file-name-extension the-link) "kra") "krita --nosplash"
                       (if (string= (file-name-extension the-link) "blend") "blender")))
  (async-shell-command (concat the-command " '" the-link "'"))
  )

(add-to-list 'display-buffer-alist '("^*Async Shell Command*" . (display-buffer-no-window)))

(map! :leader
      :desc "Insert a screenshot"
;;      "i s" 'my-org-screenshot)
      "i s" 'org-download-screenshot)

(map! :leader
      :desc "Insert image from clipboard"
;;      "i p" 'my-org-paste)
      "i p" 'org-download-clipboard)

(map! :leader
      :desc "Create a new file from a template and insert a link at point"
      "i t" 'my-org-new-file-from-template)

(map! :leader
      :desc "Open the link at point using mimeo"
      "o o" 'my-better-link-opener)

;;;------ Org roam configuration ------;;;

(setq org-roam-directory "~/Roam"
      org-roam-db-location "~/Roam/org-roam.db")

(setq org-roam-node-display-template
      "${title:65}📝${tags:*}")

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
            org-roam-db-location "~/Roam/org-roam.db"
            org-directory "~/Roam")
      (setq org-roam-directory (concat "~/" org-roam-db-choice "/Roam")
            org-roam-db-location (concat "~/" org-roam-db-choice "/Roam/org-roam.db")
            org-directory (concat "~/" org-roam-db-choice "/Roam")))
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

(map! :leader
      :prefix ("N" . "org-roam notes")
      :desc "Update current org-roam database"
      "u" 'org-roam-db-sync)

(map! :leader
      :prefix ("N" . "org-roam notes")
      :desc "Visualize org-roam database with org-roam-ui"
      "v" 'org-roam-ui-open)

(map! :leader
      :prefix ("N" . "org-roam notes")
      :desc "Re-zoom on current node in org-roam-ui"
      "z" 'org-roam-ui-node-zoom)

(org-roam-db-autosync-mode)

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
  (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "${title}\n")
  :unnarrowed t))))

(use-package org-roam-dblocks
  :hook (org-mode . org-roam-dblocks-autoupdate-mode))

;;;------ Org agenda configuration ------;;;

;; Set span for agenda
(setq org-agenda-span 1
      org-agenda-start-day "+0d")

;; Set folder for my org agenda files
(setq org-agenda-files (list "/home/emmet/Family.s/Agenda"
                             "/home/emmet/Producer.p/Agenda"
                             "/home/emmet/Producer.p/Roam"
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

(require 'org-super-agenda)

(setq org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Home Tech"
                ;; Single arguments given alone
                :and(
                    :file-path "emmet/Agenda"
                    :not (:tag "event"))
                :order 3)
         (:name "Family"
                ;; Single arguments given alone
                :and(
                    :file-path "Family.s"
                    :not (:tag "event"))
                :order 3)
         (:name "Teaching Prep"
                ;; Single arguments given alone
                :and(
                    :file-path "Teaching.p"
                    :tag "planning"
                    :not (:tag "grading"))
                :order 3)
         (:name "Teaching Secretarial"
                ;; Single arguments given alone
                :and(
                    :file-path "Teaching.p"
                    :tag "secretarial"
                    :not (:tag "grading"))
                :order 3)
         (:name "Teaching Grading"
                ;; Single arguments given alone
                :and(
                    :file-path "Teaching.p"
                    :tag "grading"
                    :not (:tag "planning"))
                :order 3)
         (:name "School Side Projects"
                :and(
                    :file-path "Teaching.p"
                    :tag "tech"
                    :not (:tag "planning"))
                :order 3)
         (:name "Gamedev Current Projects"
                ;; Single arguments given alone
                :and (
                    :file-path "Gamedev.p"
                    :todo "STRT")
                :order 5)
         (:name "Youtube"
                ;; Single arguments given alone
                :tag "youtube"
                :order 6)
         (:name "Learning"
                ;; Single arguments given alone
                :tag "learning"
                :order 7)
          (:name "Today"  ; Optionally specify section name
                :time-grid t
                :date today
                :scheduled today
                :order 1)
       ))

(map! :leader
      :desc "Open org QL view"
      "o q v" #'org-ql-view)

(map! :leader
      :desc "Open org QL view dispatcher"
      "o q d" #'org-ql-view-dispatch)

(map! :desc "Next line"
      :map org-super-agenda-header-map
      "j" 'org-agenda-next-line)

(map! :desc "Next line"
      :map org-super-agenda-header-map
      "k" 'org-agenda-previous-line)

;;;------ Org QL configuration ------;;;
;; This function block by hrehfeld on GitHub
(cl-defun org-dblock-write:my-org-ql (params)
    "Insert content for org-ql dynamic block at point according to PARAMS.
Valid parameters include:
 :scope    The scope to consider for the Org QL query. This can
            be one of the following:
            `buffer'              the current buffer
            `org-agenda-files'    all agenda files
            `org-directory'       all org files
            `(\"path\" ...)'      list of buffer names or file paths
            `all'                 all agenda files, and org-mode buffers

  :query    An Org QL query expression in either sexp or string
            form.

  :columns  A list of columns, including `heading', `todo',
            `property',`priority',`deadline',`scheduled',`closed'.
            Each column may also be specified as a list with the
            second element being a header string.  For example,
            to abbreviate the priority column: (priority \"P\").
            For certain columns, like `property', arguments may
            be passed by specifying the column type itself as a
            list.  For example, to display a column showing the
            values of a property named \"milestone\", with the
            header being abbreviated to \"M\":

              ((property \"milestone\") \"M\").

  :sort     One or a list of Org QL sorting methods
            (see `org-ql-select').

  :take     Optionally take a number of results from the front (a
            positive number) or the end (a negative number) of
            the results.

  :ts-format  Optional format string used to format
              timestamp-based columns.

For example, an org-ql dynamic block header could look like:

  #+BEGIN: org-ql :query (todo \"UNDERWAY\") :columns (priority todo heading) :sort (priority date) :ts-format \"%Y-%m-%d %H:%M\""
    (-let* (((&plist :scope :query :columns :sort :ts-format :take) params)
            (query (cl-etypecase query
                     (string (org-ql--query-string-to-sexp query))
                     (list  ;; SAFETY: Query is in sexp form: ask for confirmation, because it could contain arbitrary code.
                      (org-ql--ask-unsafe-query query)
                      query)))
            (columns (or columns '(heading todo (priority "P"))))
            (scope (cond ((and (listp scope) (seq-every-p #'stringp scope)) scope)
                         ((string-equal scope "org-agenda-files") (org-agenda-files))
                         ((or (not scope) (string-equal scope "buffer")) (current-buffer))
                         ((string-equal scope "org-directory") (org-ql-search-directories-files))
                         (t (user-error "Unknown scope '%s'" scope))))
            ;; MAYBE: Custom column functions.
            (format-fns
             ;; NOTE: Backquoting this alist prevents the lambdas from seeing
             ;; the variable `ts-format', so we use `list' and `cons'.
             (list (cons 'todo (lambda (element)
                                 (org-element-property :todo-keyword element)))
                   (cons 'heading (lambda (element)
                                    (cond
                                     ((and org-id-link-to-org-use-id
                                           (org-element-property :ID element))
                                      (org-make-link-string (format "id:%s" (org-element-property :ID element))
                                                            (org-element-property :raw-value element)))
                                     ((org-element-property :file element)
                                      (org-make-link-string (format "file:%s::*%s"
                                                                    (org-element-property :file element)
                                                                    (org-element-property :raw-value element))
                                                            (org-element-property :raw-value element)))
                                     (t (org-make-link-string (org-element-property :raw-value element)
                                                              (org-link-display-format
                                                               (org-element-property :raw-value element)))))
                                    ))
                   (cons 'priority (lambda (element)
                                     (--when-let (org-element-property :priority element)
                                       (char-to-string it))))
                   (cons 'deadline (lambda (element)
                                     (--when-let (org-element-property :deadline element)
                                       (ts-format ts-format (ts-parse-org-element it)))))
                   (cons 'scheduled (lambda (element)
                                      (--when-let (org-element-property :scheduled element)
                                        (ts-format ts-format (ts-parse-org-element it)))))
                   (cons 'closed (lambda (element)
                                   (--when-let (org-element-property :closed element)
                                     (ts-format ts-format (ts-parse-org-element it)))))
                   (cons 'property (lambda (element property)
                                     (org-element-property (intern (concat ":" (upcase property))) element)))))
            (elements (org-ql-query :from scope
                                    :where query
                                    :select '(org-element-put-property (org-element-headline-parser (line-end-position)) :file (buffer-file-name))
                                    :order-by sort)))
      (when take
        (setf elements (cl-etypecase take
                         ((and integer (satisfies cl-minusp)) (-take-last (abs take) elements))
                         (integer (-take take elements)))))
      (cl-labels ((format-element
                   (element) (string-join (cl-loop for column in columns
                                                   collect (or (pcase-exhaustive column
                                                                 ((pred symbolp)
                                                                  (funcall (alist-get column format-fns) element))
                                                                 (`((,column . ,args) ,_header)
                                                                  (apply (alist-get column format-fns) element args))
                                                                 (`(,column ,_header)
                                                                  (funcall (alist-get column format-fns) element)))
                                                               ""))
                                          " | ")))
        ;; Table header
        (insert "| " (string-join (--map (pcase it
                                           ((pred symbolp) (capitalize (symbol-name it)))
                                           (`(,_ ,name) name))
                                         columns)
                                  " | ")
                " |" "\n")
        (insert "|- \n")  ; Separator hline
        (dolist (element elements)
          (insert "| " (format-element element) " |" "\n"))
        (delete-char -1)
        (org-table-align))))

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

;;;------ dired configuration ------;;;

(map! :desc "Increase font size"
      "C-=" 'text-scale-increase)

(map! :desc "Decrease font size"
      "C--" 'text-scale-decrease)

;;;------ ranger configuration ------;;;

(map! :map ranger-mode-map
      :desc "Mark current file"
      "m" 'ranger-mark)

(map! :map ranger-mode-map
      :desc "Toggle mark on current file"
      "x" 'ranger-toggle-mark)

(map! :leader
      :desc "Open ranger"
      "o d" 'ranger)

;;;------ elfeed configuration ------;;;

(map! :leader
      :desc "Open elfeed"
      "o n" #'elfeed)

;;;------ mu4e configuration ------;;;

;; Auto-load mu4e and org-mu4e on start
(require 'mu4e-config)

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
;;(setq mu4e-marks (remove-nth-element 5 mu4e-marks))
;;(add-to-list 'mu4e-marks
;;     '(trash
;;       :char ("d" . "▼")
;;       :prompt "dtrash"
;;       :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
;;       :action (lambda (docid msg target)
;;                 (mu4e~proc-move docid
;;                    (mu4e~mark-check-target target) "-N"))))

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
