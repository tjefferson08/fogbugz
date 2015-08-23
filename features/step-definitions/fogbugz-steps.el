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

(Given "I am in the filters list buffer"
       (lambda ()
         (switch-to-buffer (get-buffer-create "*fogbugz-filters-list*"))
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert
            "My Cases\n"
            "Cases I should have closed months ago\n"
            "* Customer Service Top 10\n"))
         (fogbugz-filter-list-mode)))

(Given "I am in the search results buffer"
       (lambda ()
         (switch-to-buffer (get-buffer-create "*fogbugz-search-results*"))
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert
            "1234\n"
            "Title: Fix things\n"
            "Priority: High\n"
            "Status: Active\n"
            "Project: Some Project\n"
            "LatestTextSummary: Do it already!\n"
            "\n"
            "4321\n"
            "Title: Fire intern"
            "Priority: High\n"
            "Status: Active\n"
            "Project: Some Project\n"
            "LatestTextSummary: He sucks\n")

         (fogbugz-search-results-mode))))

(Given "the mock is set for \"\\(.+\\)\"$"
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
           (get-buffer "xml-mock")))

        ((equal method "case")

         (defun url-retrieve-synchronously (url)
           (setq url-http-end-of-headers 13)
           (switch-to-buffer (get-buffer-create "xml-mock"))
           (insert "FAKE HEADERS\n\n"
                   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                   "<response>\n"
                   "  <cases count=\"1\">\n"
                   "    <case ixBug=\"1234\">\n"
                   "      <ixBug>1234</ixBug>\n"
                   "      <sTitle>Fire the intern</sTitle>\n"
                   "      <sPriority>High</sPriority>\n"
                   "      <events>\n"
                   "        <event ixBugEvent=\"123\" ixBug=\"1234\">\n"
                   "          <evtDescription>Opened by Bob</evtDescription>\n"
                   "        </event>\n"
                   "        <event ixBugEvent=\"123\" ixBug=\"1234\">\n"
                   "          <evtDescription>Assigned to Bill by Bob</evtDescription>\n"
                   "        </event>\n"
                   "      </events>\n"
                   "    </case>\n"
                   "  </cases>\n"
                   "</response>\n")
           (get-buffer "xml-mock"))))))

(And "I inspect the variable \"\\(.+\\)\"$"
     (lambda (variable-name-string)
       (shut-up
         (describe-variable (intern variable-name-string))
         (switch-to-buffer "*Help*"))))

