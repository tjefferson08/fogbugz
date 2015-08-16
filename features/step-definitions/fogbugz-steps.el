;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

;; (Given "I am in buffer" "\\(.+\\)\"$"
;;        (lambda (buffer-name)
;;       switch
;;       ))

(Given "I am logged on"
       (lambda ()
         (setq fogbugz-domain "example.fogbugz.com")
         (setq fogbugz-email "email@example.com")
         (setq fogbugz-password "secret")
         (setq fogbugz-token "abcde")))

(And "the mock is set for \"\\(.+\\)\"$"
     (lambda (method)

       (cond
        ((equal method "logon")

         ;; nuclear "mock", just redefine the whole dang function
         (defun url-retrieve-synchronously (url)
           (setq url-http-end-of-headers 13)
           (switch-to-buffer (get-buffer-create "xml-mock"))
           (insert "FAKE HEADERS<?xml version=\"1.0\" encoding=\"UTF-8\"?><response><token>123</token></response>")
           (get-buffer "xml-mock")))

        ((equal method "listFilters")

         (defun url-retrieve-synchronously (url)
           (setq url-http-end-of-headers 13)
           (switch-to-buffer (get-buffer-create "xml-mock"))
           (insert "FAKE HEADERS\n\n"
                   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                   "<response>\n"
                   "<filters>\n"
                   "<filter type=\"builtin\" sFilter=\"ez349\">My Cases</filter>\n"
                   "<filter type=\"saved\" sFilter=\"304\">Cases I should have closed months ago</filter>\n"
                   "<filter type=\"shared\" sFilter=\"98\" status=\"current\">Customer Service Top 10</filter>\n"
                   "</filters>\n"
                   "</response>\n")
           (get-buffer "xml-mock"))))))

(And "I inspect the variable \"\\(.+\\)\"$"
     (lambda (variable-name-string)
       (message (symbol-value (intern variable-name-string)))))
