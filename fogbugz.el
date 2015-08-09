(defun fogbugz-base-uri (method)
  "Get the fogbugz web API URI for the provided method."

  (if (or (not method)
          (eq (length method) 0))
      (error "METHOD must be a valid API method name"))

  (if (or (not fogbugz-domain)
          (eq (length fogbugz-domain) 0))
      (error "`fogbugz-domain' must be set to a valid hostname"))
  (concat "https://" fogbugz-domain "/api.asp?cmd=" method))

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
  if FORCE-REFRESH is truthy. Request synchronously and store
  token in `fogbugz-token'"

  (interactive)
  (if (or force-refresh
            (not (boundp 'fogbugz-token))
            (not fogbugz-token))
      (let* ((response (fogbugz-request
                        "logon"
                        `(("email" . ,fogbugz-email)
                          ("password" . ,fogbugz-password))))
             (token (fogbugz-parse-logon-response response)))
        (setq fogbugz-token token)
        token)
    fogbugz-token))

(defun fogbugz-request (endpoint params)
  "Make a request to ENDPOINT, providing PARAMS (which can be
  nil) as POST data. Respond synchronously with parsed XML data
  from `libxml-parse-xml-region'"
  (message "request!")
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))

        ;; POST data, form-encoded
        (url-request-data
         (if params
             (mapconcat
              (lambda (param-pair)
                (concat (url-hexify-string (car param-pair)) "="
                        (url-hexify-string (cdr param-pair))))
              params
              "&")
           nil))

        (url (concat (fogbugz-base-uri endpoint))))

        (message "URL: %s" url)
        (switch-to-buffer (url-retrieve-synchronously url))
        (let ((data (libxml-parse-xml-region (point) (point-max))))
          (kill-buffer)
          data)))
;; (setq url-debug t)

(defun fogbugz-parse-list-filters-response (response)
  "Turn raw RESPONSE into a list of filter lists."
  (cdr (cdr
        (car (cdr (cdr response))))))

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
           "listFilters"
           `(("token" . ,fogbugz-token))))))
    (setq fogbugz-filter-list filter-list))
  (switch-to-buffer "*fogbugz-filters-list*")
  (read-only-mode -1)
  (erase-buffer)
  (loop for filter in fogbugz-filter-list do
        (insert (assoc-default 'sFilter (fogbugz-get-filter-attributes filter)) " - ")
        (insert (if (equal
                     (assoc-default 'status (fogbugz-get-filter-attributes filter))
                     "current")
                    "* "
                  "")) ;; mark currently active filter
        (insert (fogbugz-get-filter-name filter)) ;; insert filter name (string)
        (insert "\n"))
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
  "Set S-FILTER (tring) as current filter in fogbugz."
  ;; (let ((s-filter-string
  ;;     (cond
  ;;      ((numberp s-filter)
  ;;       (message "number")
  ;;       (number-to-string s-filter))
  ;;      ((integerp s-filter)
  ;;       (message "int")
  ;;       (int-to-string s-filter))
  ;;      ((stringp s-filter)
  ;;       (message "string"))
  ;;      (t (message "else"))))))

  (pp
   (fogbugz-request "setCurrentFilter" `(("token" . ,fogbugz-token)
                                         ("sFilter" . ,s-filter)))))



(define-derived-mode fogbugz-filter-list-mode fundamental-mode "fbz-filter"
  "Fogbugz filter list mode
\\{fogbugz-filter-list-mode-map}"
  (read-only-mode 1))

(define-key fogbugz-filter-list-mode-map (kbd "q") 'quit-window)
(define-key fogbugz-filter-list-mode-map (kbd "n") 'next-line)
(define-key fogbugz-filter-list-mode-map (kbd "p") 'previous-line)
(define-key fogbugz-filter-list-mode-map (kbd "<RET>") 'fogbugz-set-filter-under-point)


(defvar temp-filter `(filter
                      ((type . "saved")
                       (sFilter . "10")
                       (status . "current"))
                      "Travis's Bugz"))

(provide 'fogbugz-mode)
