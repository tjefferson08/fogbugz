;; `fogbugz-base-uri'
(ert-deftest fogbugz-base-uri-test ()
  (let ((fogbugz-domain "some-domain.com"))
    (should (equal
             (fogbugz-base-uri "some-method")
             "https://some-domain.com/api.asp?cmd=some-method"))))

(ert-deftest fogbugz-base-uri-error-test ()
  (let ((fogbugz-domain "some-domain.com"))
    (should-error
     (fogbugz-base-uri nil)))

  (let ((fogbugz-domain nil))
    (should-error
     (fogbugz-base-uri "some-method"))))

;; `fogbugz-logon'
(ert-deftest fogbugz-logon-request-test ()
  (with-mock
    (mock (fogbugz-request
           "logon"
           '(("email" . "email@email.com") ("password" . "secret")))
          => "raw-data")
    (mock (fogbugz-parse-logon-response "raw-data") => "parsed-token")

    (let ((fogbugz-token nil)
          (fogbugz-email "email@email.com")
          (fogbugz-password "secret"))
      (should (equal (fogbugz-logon) "parsed-token")))))

(ert-deftest fogbugz-logon-no-request-test ()
  (with-mock
    (not-called fogbugz-request)
    (not-called fogbugz-parse-logon-response)

    (let ((fogbugz-token "already-set"))
      (should (equal (fogbugz-logon) "already-set")))))

;; `fogbugz-request'
(ert-deftest fogbugz-request-exclude-token-test ()
  (with-mock
    (let ((xml-buffer (generate-new-buffer "mock-xml-buffer"))

          ;; fake headers end at char 13 in xml data
          (url-http-end-of-headers 13))

      (save-excursion
        (switch-to-buffer xml-buffer)
        (insert "FAKE HEADERS<?xml version=\"1.0\" encoding=\"UTF-8\"?><response><data>123</data></response>"))

      (mock (url-retrieve-synchronously "<mock url>") => xml-buffer)
      (mock (fogbugz-base-uri "endpoint-name") => "<mock url>")
      (should (equal
               (fogbugz-request "endpoint-name" `(("param1" . "value1") ("param2" . "value2")) t)

               ;; mock xml parsed into lisp object
               '(response nil (data nil "123")))))))
;; `fogbugz-search'
(ert-deftest fogbugz-search-no-params ()
  (with-mock
   (mock (fogbugz-request
          "search"
          '(("q" . "")
            ("cols" . "sTitle,sPriority,sStatus,sProject,sLatestTextSummary")
            ("max" . "50")))
         => '(response nil (data1 "data1") (data2 "data2")))

   (should (equal
            (fogbugz-search)
            '((data1 "data1") (data2 "data2"))))))

(ert-deftest fogbugz-search-all-params ()
  (with-mock
   (mock (fogbugz-request
          "search"
          '(("q" . "query") ("cols" . "columns") ("max" . "42")))
         => '(response nil (data1 "data1") (data2 "data2")))

   (should (equal
            (fogbugz-search "query" "columns" "42")
            '((data1 "data1") (data2 "data2"))))))

;; `fogbugz-humanize-attribute-name'
(ert-deftest fogbugz-humanize-attribute-name-test ()
  (should (equal
           (fogbugz-humanize-attribute-name 'lowercasetypeCamelCaseAttribute)
           "CamelCaseAttribute")))

(ert-deftest fogbugz-humanize-attribute-name-test ()
  (should (equal
           (fogbugz-humanize-attribute-name 'lowercaseonly)
           "lowercaseonly")))

;; `fogbugz-get-case-number-under-point'
(ert-deftest fogbugz-get-case-number-under-point-success ()
  (with-temp-buffer
    (insert "    1234: Some case")

    (goto-char (point-min))
    (should (equal
             (fogbugz-get-case-number-under-point)
             "1234"))

    (end-of-line)
    (should (equal
             (fogbugz-get-case-number-under-point)
             "1234"))))

(ert-deftest fogbugz-get-case-number-under-point-fail ()
  (with-temp-buffer
    (insert " Something that doesn't match ")

    (goto-char (point-min))
    (should (null (fogbugz-get-case-number-under-point)))

    (end-of-line)
    (should (null (fogbugz-get-case-number-under-point)))))

