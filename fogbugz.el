(require 'cl)

(defun fogbugz-base-uri (method)
  "Get the fogbugz web API URI for the provided method."

  (if (or (not method)
          (eq (length method) 0))
      (error "METHOD must be a valid API method name"))

  (if (or (not fogbugz-domain)
          (eq (length fogbugz-domain) 0))
      (error "`fogbugz-domain' must be set to a valid hostname"))
  (concat "https://" fogbugz-domain "/api.asp?cmd=" method))

(defun fogbugz-prompt-for-credentials ()
  "Prompt for `fogbugz-domain', `fogbugz-email', and
`fogbugz-password', respectively. There are many ways to set/bind
these variables; feel free to skip this function and set them as
you please."

  (interactive)

  (setq fogbugz-domain
        (read-from-minibuffer "Fogbugz domain (e.g. some-company.fogbugz.com): "))

  (setq fogbugz-email
        (read-from-minibuffer "Fogbugz login email: "))

  (setq fogbugz-password
        (read-from-minibuffer "Fogbugz login password: ")))

(defun fogbugz-refresh-token ()
  "Force a token refresh."

  (interactive)
  (fogbugz-logon t))

(defun fogbugz-parse-logon-response (response)
  "Parse response from logon request."
  (nth 2 (nth 2 response)))

(defun fogbugz-logon (&optional force-refresh)
  "Log in with `fogbugz-email' and `fogbugz-password'
  variables. Only perform request when `fogbugz-token' is nil, or
  if FORCE-REFRESH is true. Request synchronously and store
  token in `fogbugz-token'"

  (interactive)
  (if (or force-refresh
          (not (boundp 'fogbugz-token))
          (not fogbugz-token))
      (let* ((response (fogbugz-request
                        "logon"
                        `(("email" . ,fogbugz-email)
                          ("password" . ,fogbugz-password))
                        t))
             (token (fogbugz-parse-logon-response response)))
        (setq fogbugz-token token)
        token)
    fogbugz-token))

(defun fogbugz-request (endpoint &optional params &optional exclude-token)
  "Make a request to ENDPOINT, providing PARAMS (which can be
  nil) as POST data. Respond synchronously with parsed XML data
  from `libxml-parse-xml-region'. Since most requests will need
  to provide `fogbugz-token', automatically include that in
  PARAMS unless EXCLUDE_TOKEN is true."

  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))

        ;; POST data, form-encoded
        (url-request-data (let ((url-params (if exclude-token
                                                params
                                              (cons `("token" . ,fogbugz-token) params))))
                            (mapconcat
                             (lambda (param-pair)
                               (concat (url-hexify-string (car param-pair)) "="
                                       (url-hexify-string (cdr param-pair))))
                             url-params
                             "&")))

        (url (concat (fogbugz-base-uri endpoint))))

    (message "URL: %s\nDATA: %s" url url-request-data)

    (switch-to-buffer (url-retrieve-synchronously url))

    ;; special marker set by `url-retrieve-synchronously'
    (goto-char url-http-end-of-headers)

    ;; move past any blank lines
    (while (looking-at "\\s-*$")
      (forward-line 1)
      (beginning-of-line))

    (let ((data (libxml-parse-xml-region (point) (point-max))))
      (kill-buffer)
      data)))

(defun fogbugz-parse-list-filters-response (response)
  "Turn raw RESPONSE into a list of filter lists."
  (nthcdr 2 (car (nthcdr 2 response))))

(defun fogbugz-get-filter-name (filter)
  (nth 2 filter))

(defun fogbugz-get-filter-attributes (filter)
  "Get attributes from FILTER as alist."
  (nth 1 filter))

(defun fogbugz-list-filters ()
  (interactive)
  (let ((filter-list
         (fogbugz-parse-list-filters-response
          (fogbugz-request
           "listFilters"))))

    ;; store filter list in a variable for easy access
    (setq fogbugz-filter-list filter-list))

  (switch-to-buffer "*fogbugz-filters-list*")
  (read-only-mode -1)
  (erase-buffer)
  (loop for filter in fogbugz-filter-list do
        (insert (if (equal
                     (assoc-default 'status (fogbugz-get-filter-attributes filter))
                     "current")
                    "* "
                  "")) ;; mark currently active filter
        (insert (fogbugz-get-filter-name filter)) ;; insert filter name (string)
        (insert " - " (assoc-default 'sFilter (fogbugz-get-filter-attributes filter)))
        (insert "\n"))
  (goto-char (point-min))
  (fogbugz-filter-list-mode))

(defun fogbugz-set-filter-under-point ()
  "Set filter under point as current filter in Fogbugz. Use
`line-number-at-pos' to index into `fogbugz-filter-list' for
sFilter ID."

  (interactive)
  (let* ((index (1- (line-number-at-pos)))
         (filter-to-activate (nth index fogbugz-filter-list))
         (s-filter-to-activate (assoc-default 'sFilter (nth 1 filter-to-activate))))
    (message "activating filter %s with sFilter %s"
             (fogbugz-get-filter-name filter-to-activate)
             s-filter-to-activate)
    (fogbugz-set-current-filter s-filter-to-activate)))

(defun fogbugz-set-current-filter (s-filter)
  "Set S-FILTER (string) as current filter in fogbugz."

  (fogbugz-request "setCurrentFilter" `(("sFilter" . ,s-filter))))

(defun fogbugz-search (&optional query column-names max)
  (interactive)

  ;; parse off top-level <response> tag
  (cdr
   (cdr
    (fogbugz-request
     "search"
     `(("q" . ,(or query ""))
       ("cols" . "sTitle,sPriority,sStatus,sProject,sLatestTextSummary")
       ("max" . "50"))))))

(defun fogbugz-list-search-results (search-results &optional results-buffer)
  (let ((target-buffer (or
                        results-buffer
                        (get-buffer-create "*fogbugz-search-results*")))
        (description (nth 2 (car search-results)))
        (case-list (cdr (cdr (nth 2 search-results)))))
    (switch-to-buffer target-buffer)
    (read-only-mode -1)
    (erase-buffer)
    (goto-char (point-min))
    (insert "* " description  " *\n\n")

    (loop for case in case-list do
          (let ((case-attrs (nth 1 case))
                (remaining-fields (cdr (cdr case))))

            (insert (assoc-default 'ixBug case-attrs) "\n")
            (loop for field in remaining-fields do
                  (insert (fogbugz-humanize-attribute-name (nth 0 field)) ": ")
                  (insert (or (nth 2 field) "") "\n"))
            (insert "\n"))))
  (fogbugz-search-results-mode)
  (goto-char (point-min)))

(defun fogbugz-select-filter-under-point ()
  (interactive)
  (let* ((index (1- (line-number-at-pos)))
         (filter-to-activate (nth index fogbugz-filter-list))
         (s-filter-to-activate (assoc-default 'sFilter (nth 1 filter-to-activate))))
    (message "activating filter %s with sFilter %s"
             (fogbugz-get-filter-name filter-to-activate)
             s-filter-to-activate)
    (fogbugz-set-current-filter s-filter-to-activate)
    (fogbugz-list-search-results
     (fogbugz-search))))

(defun fogbugz-start-work-on-case (&optional ix-bug)
  "Beware: creates a new interval"
  (interactive)

  (let ((bug-id (or ix-bug
                    (read-from-minibuffer "Bug: "))))
    (save-excursion
      (message "bug id %s" bug-id)
      (pp (fogbugz-request "startWork" `(("ixBug" . ,bug-id)))))))


(defun fogbugz-work-on-case-under-point ()
  (interactive)
  (save-excursion
    (let* ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          (case-number
           (match-string
            (string-match "^[ ]*\\([0-9]+\\)[ ]*" line)
            line)))
      (fogbugz-start-work-on-case case-number))))

(defun fogbugz-stop-work ()
  (interactive)
  (pp (fogbugz-request "stopWork")))

(defun fogbugz-humanize-attribute-name (attribute-symbol)
  "Translates ATTRIBUTE-SYMBOL for human consumption:
E.g. (fogbugz-humanize-attribute-name 'ixBug) ;; => \"bug\""

  (let ((name (symbol-name attribute-symbol))
        (case-fold-search nil))
    (substring name (or
                     (string-match "[A-Z]" name)
                     0))))

(define-derived-mode fogbugz-filter-list-mode fundamental-mode "fbz-filter"
  "Fogbugz filter list mode
\\{fogbugz-filter-list-mode-map}"
  (read-only-mode 1))

(define-key fogbugz-filter-list-mode-map (kbd "q") 'quit-window)
(define-key fogbugz-filter-list-mode-map (kbd "n") 'next-line)
(define-key fogbugz-filter-list-mode-map (kbd "p") 'previous-line)
(define-key fogbugz-filter-list-mode-map (kbd "r") 'fogbugz-list-filters)
(define-key fogbugz-filter-list-mode-map (kbd "c") 'fogbugz-set-filter-under-point)
(define-key fogbugz-filter-list-mode-map (kbd "<RET>") 'fogbugz-select-filter-under-point)

(define-derived-mode fogbugz-search-results-mode fundamental-mode "fbz-results"
  "Fogbugz search results mode
\\{fogbugz-search-results-mode-map}"
  (read-only-mode 1)
  (setq font-lock-defaults '(`(("^[0-9]+" . font-lock-function-name-face)
                               ("^[a-zA-z0-9]+:" . font-lock-variable-name-face)))))

(define-key fogbugz-search-results-mode-map (kbd "q") 'quit-window)
(define-key fogbugz-search-results-mode-map (kbd "n") 'next-line)
(define-key fogbugz-search-results-mode-map (kbd "p") 'previous-line)
(define-key fogbugz-search-results-mode-map (kbd "w") 'fogbugz-work-on-case-under-point)
(define-key fogbugz-search-results-mode-map (kbd "s") 'fogbugz-stop-work)
(define-key fogbugz-search-results-mode-map (kbd "<RET>") (lambda () (interactive) (message "LOL")))

(provide 'fogbugz)
