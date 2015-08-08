(defun fogbugz-base-uri (method)
  "Get the fogbugz web API URI for the provided method."

  (concat "https://" fogbugz-domain "/api.asp?cmd=" method))

(defun fogbugz-get-token (force-refresh)
  "Log in with `fogbugz-email' and `fogbugz-password'
  variables. Only perform request when `fogbugz-token' is nil, or
  if FORCE-REFRESH is truthy. Request asynchronously and store
  token in `fogbugz-token'"

  (interactive)

  (if (or force-refresh (not fogbugz-token))
      (fogbugz-request
       "logon"
       `(("email" . ,fogbugz-email)
         ("password" . ,fogbugz-password))
       `(lambda (response)
          (setq fogbugz-token
                (nth 2 (nth 2 (nth 0 response))))))))

(defun fogbugz-request (endpoint params callback)
  "Make a request to ENDPOINT, providing PARAMS (which can be
  nil) as POST data. Call callback with a single argument, the
  parsed XML data from `xml-parse-region'"

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

    (message "url %s" url)
    (url-retrieve
     url
     `(lambda (status)

        ;; in url response buffer
        (goto-char url-http-end-of-headers)
        (forward-line)
        ;; Invoke callback with data parsed from buffer. Note that the
        ;; callback's active buffer will be the temporary buffer used
        ;; by the url lib.
        (let ((data (xml-parse-region (point) (point-max))))
          (funcall (function ,callback) data)))

     ;; no callback args
     nil

     ;; silent
     t)))
