;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Sane defaults
(defvar is-archie-p (equal (system-name) "archie"))
(defvar is-macos-p (equal (system-name) "Shawns-Macbook-Pro.local"))
(setq-default window-combination-resize t)
(setq global-auto-revert-non-file-buffers t)
(setq undo-limit 80000000       ; Raise undo-limit to 80Mb
      evil-want-fine-undo t     ; By default, all changes while in insert mode are one big blob. Make it granular.
      password-cache-expiry nil ; Trust your computers.
      scroll-margin 2           ; It's nice to maintain a little margin
      window-resize-pixelwise t
      )
(setq auth-sources "~/.authinfo.gpg")

(display-time-mode 1)
(global-subword-mode 1)
(electric-pair-mode t)

(setq-default major-mode 'org-mode)
(setq-default line-spacing 10)
;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Shawn Borton"
      user-mail-address "shawn@shawnborton.info")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(use-package! all-the-icons)
(setq doom-font (font-spec :family "JetBrains Mono" :style "Regular" :size 14)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 18)
      doom-big-font (font-spec :family "JetBrains Mono" :style "Regular" :size 26))
(defvar sb-org-font (font-spec :family "Source Code Pro" :style "Regular" :size 14))

(setq doom-theme 'doom-nord)
(after! doom-themes
  ;;(load-theme 'doom-nano-dark t)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")
(setq which-key-idle-delay 0.5) ;; Faster popups

(use-package! all-the-icons-dired)

;; Major modes in which to display word count continuously.
;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;; remove the modes from `doom-modeline-continuous-word-count-modes'.
;;
(after! doom-modeline
  (setq doom-modeline-height 18)
  (setq doom-modeline-bar-width 6)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode org-mode)))


(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))
(setq +ivy-buffer-preview t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!
(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead if not using dired-open package
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete)
(evilem-default-keybindings "SPC")

(defun sbinfo/thunar-current-dir ()
  "Open the current directory in Thunar."
  (interactive)
  (let ((default-directory (if (buffer-file-name)
                                (file-name-directory (buffer-file-name))
                              default-directory)))
    (start-process "Thunar" nil "thunar" default-directory)))

(defun sbinfo/my-find-file-with-pattern (dir pattern)
  "Find files containing PATTERN in directory DIR and open them."
  (interactive "DEnter directory: \nsEnter pattern: ")
  (let* ((files (directory-files dir))
         (matches (seq-filter (lambda (f) (string-match-p pattern f)) files)))
    (dolist (filename matches)
      (find-file (concat (file-name-as-directory dir) filename)))))

;; EWW Stuff
(setq browse-url-browser-function 'eww-browse-url)
(map! :leader
      :desc "Eww web browser"
      "e w" #'eww
      :leader
      :desc "Eww reload page"
      "e R" #'eww-reload
      :leader
      :desc "Search web for text between BEG/END"
      "s w" #'eww-search-words)

(after! neotree
  (setq neo-smart-open t
        doom-themes-neotree-enable-variable-pitch nil
        neo-window-fixed-size nil))

(map! :leader
      :desc "Toggle neotree file viewer"
      "t n" #'neotree-toggle
      :leader
      :desc "Open directory in neotree"
      "d n" #'neotree-dir)

;; Development keybindings
(map! :leader
      :desc "npm install"
      "c n i" #'npm-mode-npm-install
      :leader
      :desc "npm run"
      "c n r" #'npm-mode-npm-run)

;; (map! :leader
;;      :desc "Edit agenda file"
;;     "- a" #'(lambda () (interactive) (find-file "~/Org/agenda.org"))
(map! :leader
      :desc "Edit eshell aliases"
      "- e" #'(lambda () (interactive) (find-file "~/.doom.d/aliases"))
      :leader
      :desc "Edit doom init.el"
      "- i" #'(lambda () (interactive) (find-file "~/.doom.d/init.el"))
      :leader
      :desc "Edit doom config.el"
      "- c" #'(lambda () (interactive) (find-file "~/.doom.d/config.el"))
      :leader
      :desc "Edit doom packages.el"
      "- p" #'(lambda () (interactive) (find-file "~/.doom.d/packages.el")))

;; (after! mastodon
  ;; (setq mastodon-auth-source-file "~/.authinfo")
  ;; (setq mastodon-instance-url "https://distrotoot.com/"))

;; TWITTER
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)

;; RSS (elfeed)
(after! elfeed
  ;;(elfeed-org)
  ;;(setq rmh-elfeed-org-files (list "~/Dropbox/Org/feedly.org"))
  (setq elfeed-use-curl t)
  (setq elfeed-protocol-fever-update-unread-only t)
  (setq elfeed-protocol-feeds (list
                      (list "fever+https://shawnb@rss.onionpatch.cloud"
                          :api-url "https://rss.onionpatch.cloud/api/fever.php"
                          :password ")9vwibuQmv9pr"
                          :autotags '(("https://www.axios.com" news)("Hacker News" technology)))))
  ;; enable elfeed-protocol
  (setq elfeed-protocol-enabled-protocols '(fever))
  (elfeed-protocol-enable)
  (elfeed-set-timeout 36000)
  (use-package! elfeed-link))

(setq freshrss-hostname "https://rss.onionpatch.cloud")
(setq main-elfeed-feed "https://shawnb@rss.onionpatch.cloud/api/fever.php")
(defun freshrss-network-connection-p ()
  (not (condition-case nil
        (delete-process
         (make-network-process
          :name freshrss-hostname
          :host "elpa.gnu.org"
          :service 443))
      (error t))))

(defun elfeed-full-update ()
  (interactive)
  (if (freshrss-network-connection-p) (delete-directory "~/.cache/doom/elfeed" t))
  (setq elfeed-db nil)
  ;; (elfeed-protocol-fever-update main-elfeed-feed))
  (elfeed-update))
(map! :map 'elfeed-search-mode-map :desc "Update elfeed" :n "g R" #'elfeed-full-update)
(map! :leader
      :desc "Launch Elfeed"
      "r r" #'elfeed
      :leader
      :desc "Update RSS feeds"
      "r u" #'elfeed-update
      :leader
      :desc "Get Culture items"
      "r c" #'(lambda () (interactive) (elfeed-search-set-filter "@1-week-ago +unread +Culture"))
      :leader
      :desc "Get default search"
      "r d" #'(lambda () (interactive) (elfeed-search-set-filter "@1-week-ago +unread "))
      :leader
      :desc "Get Hacker News items"
      "r h" #'(lambda () (interactive) (elfeed-search-set-filter "@1-week-ago +unread =Hacker"))
      :leader
      :desc "Add entry to Pocket"
      "r k e" #'(lambda () (interactive) (pocket-reader-elfeed-entry-add-link))
      :leader
      :desc "Add search item to Pocket"
      "r k s" #'(lambda () (interactive) (pocket-reader-elfeed-search-add-link))
      :leader
      :desc "Get News items"
      "r n e" #'(lambda () (interactive) (elfeed-search-set-filter "@1-week-ago +unread +News"))
      :leader
      :desc "Get Nitters"
      "r n i" #'(lambda () (interactive) (elfeed-search-set-filter "@1-week-ago +unread +Nitters"))
      :leader
      :desc "Get Technology items"
      "r t" #'(lambda () (interactive) (elfeed-search-set-filter "@1-week-ago +unread +Technology"))
      :leader
      :desc "Get Videos"
      "r v" #'(lambda () (interactive) (elfeed-search-set-filter "@1-week-ago +unread +Videos"))
      :leader
      :desc "Get Writing items"
      "r w" #'(lambda () (interactive) (elfeed-search-set-filter "@1-week-ago +unread +Writing")))
(use-package! elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.75))

(add-hook 'elfeed-show-mode-hook 'visual-line-mode)
(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(evil-define-key 'normal elfeed-search-mode-map
  (kbd "RET") 'elfeed-goodies/split-search-show-entry
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)

(setq elfeed-goodies/entry-pane-position 'bottom)

(setq elfeed-search-filter "@1-week-ago +unread "
      elfeed-search-title-min-width 80)
      ;;elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style)

;;(defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
  ;;  "title face in elfeed show buffer"
  ;;  :group 'elfeed)

;; (after! elfeed-show
  ;; (require 'url))

(defun +rss/elfeed-show-refresh--better-style ()
  "Update the buffer to match the selected entry, using a mail-style."
  (interactive)
  (let* ((inhibit-read-only t)
         (title (elfeed-entry-title elfeed-show-entry))
         (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (author (elfeed-meta elfeed-show-entry :author))
         (link (elfeed-entry-link elfeed-show-entry))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tagsstr (mapconcat #'symbol-name tags ", "))
         (nicedate (format-time-string ("%a, %e %b %Y %T %Z" date))
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (type (elfeed-entry-content-type elfeed-show-entry))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (feed-title (elfeed-feed-title feed))
         (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
         (erase-buffer)
         (insert "\n")))
  )


;; ⌘-b      ; browse to URL in foreground
;; C-- ⌘-b  ; browse to URL in background

;; ⌘-i      ; search Google in foreground
;; C-- ⌘-i  ; search Google in background

;; position cursor on a URL
;; ⌘-b

;; select a region
;; ⌘-i

;;(require 'osx-browse)
;;(osx-browse-mode 1)

;; (after! pocket-reader
;;   (map! :map pocket-reader-mode-map
;;         "ta" #'pocket-reader-add-tags))

 (defun writing-mode ()
  (interactive)
  ;; (zen/toggle)
  (buffer-face-mode)
  (linum-mode 0)
  (blink-cursor-mode)
  (visual-line-mode 1)
  (setq truncate-lines nil
        global-hl-line-mode nil
        display-line-numbers nil))

(add-hook 'markdown-mode-hook 'writing-mode)
(setq ispell-program-name "/usr/bin/aspell")
(setq ispell-list-command "--list")

(defun sbinfo/org-mode-setup ()
  (org-indent-mode)
  (org-superstar-mode 1)
  (org-bullets-mode 1))
  ;; (variable-pitch-mode 1)
  ;; (visual-line-mode 1))

(defun sbinfo/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    ;; (set-face-attribute (car face) nil :font "JetBrains Mono" :weight 'regular :height (cdr face))))
    ;; (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height (cdr face))))
    ;; (set-face-attribute (car face) nil :font "Source Code Pro" :weight 'regular :height (cdr face)))
    (set-face-attribute (car face) nil :font "Source Code Pro" :height (cdr face))))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    ;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    ;; (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
    ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    ;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    ;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defvar sbinfo/open-ai-key "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")

(after! chatgpt-shell
  (setq chatgpt-shell-openai-key sbinfo/open-ai-key))

(after! org-ai
  (add-hook 'org-mode-hook #'org-ai-mode)
  (org-ai-install-yasnippets)
  (setq org-ai-use-auth-source nil)
  (setq org-ai-openai-api-token sbinfo/open-ai-key))

(use-package! gptel
 :config
 (setq! gptel-api-key sbinfo/open-ai-key))

(after! org
  (setq org-directory "~/org/"
        org-agenda-files '("agenda.org" "goals_2024.org" "job_search_2022.org")
        org-agenda-start-with-follow-mode t
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")
        org-indent-indentation-per-level 4
        org-log-done 'time
        org-roam-directory "~/org/roam/"
        ;;org-journal-dir "~/Dropbox/Org/journal/"
        ;;org-journal-date-format "%B %d, %Y (%A) "
        ;;org-journal-file-format "%Y-%m-%d.org"
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "BLOG(b)"           ; Blog writing assignments
             "PROJ(p)"           ; A project that contains other tasks
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )) ; Task has been cancelled
        org-capture-templates
        '(("b" "Bookmark" entry
           (file "~/org/bookmarksv2.org")
           "* %^{title}\n#+date: %U\n- URL: %^{link}\n- Context: %?")
          ("t" "Personal todo" entry
           (file+headline "~/org/agenda.org" "Inbox")
           "* [ ] %?\n%i\n%a")
          ("n" "Personal notes" entry
           (file+headline "~/org/notes.org" "Inbox")
           "* %u %?\n%i\n%a" :prepend t)))
          ;;("j" "Journal" entry
           ;;(file+olp+datetree +org-capture-journal-file)
           ;;"* %U %?\n%i\n%a" :prepend t)))
  (sbinfo/org-font-setup))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(defun sbinfo/split-and-indirect-orgtree ()
  "Splilts window to the right and opens an org tree section in it."
  (interactive)
  (split-window-right)
  (windmove-right)
  (org-tree-to-indirect-buffer))

(defun sbinfo/kill-and-unsplit-orgtree ()
  "Kill the cloned buffer and deletes the window."
  (interactive)
  (kill-this-buffer)
  (delete-window))

;; (require 'org-similarity)

;; (use-package! org-similarity
;;  :config
;;  (setq org-similarity-directory org-roam-directory))
;; (add-load-path! "~/.emacs.d/.local/straight/repos/org-similarity")
;; (require 'org-similarity)
;; (setq org-similarity-python-interpreter "/home/shawnb/.emacs.d/.local/straight/repos/org-similarity/venv/bin/python")
;; (setq org-similarity-directory org-roam-directory)

;; to turn off confirmations
(setq browse-url-dwim-always-confirm-extraction nil)

;; Todoist
(setq todoist-token "344745a4ca6497cbce38cc957cad0c59c5d91613")

;; Auto create directories recursively
;; See https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
(defun er-auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs)
;; ACe Window
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
