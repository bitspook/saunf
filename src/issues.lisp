(in-package #:saunf)

(defsection @managing-issues (:title "Managing Issues")
  "An issue is a request for a new feature, a bug report etc. For example,
a Jira or Github issue. Saunf provides this abstraction to allow working with
different issue management solutions with a single interface."
  "## API"
  (issue class)

  (issue-manager class)

  (fetch-issue generic-function)
  (assigned-issues generic-function)

  (jira class))

(export-always 'issue-manager)
(defclass issue-manager ()
  nil
  (:documentation "Parent class for all issue managers."))

(defgeneric fetch-issue (manager id)
  (:documentation "Fetch issue with ID for MANAGER."))

(defgeneric assigned-issues (manager &key)
  (:documentation "Return list of issues assigned to the user."))

;;---
;;; Jira
(export-always 'jira)
(defclass jira (issue-manager)
  ((url :initarg :url
        :initform (error "Jira url is required")
        :reader jira-url
        :documentation "URL of the Jira instance")
   (user :initarg :user
         :initform (error "Jira username is required")
         :reader jira-user
         :documentation "email used for authenticating with Jira")
   (password :initarg :password
             :initform (error "Jira password is required")
             :reader jira-password
             :documentation "Password or api-key used for authenticating with Jira"))
  (:documentation "Interact with Jira projects."))

(defmethod fetch-issue ((jira jira) issue-id)
  "Fetch JIRA ISSUE with ISSUE-ID."
  (let* ((base-url (jira-url jira))
         (url (format nil "~a/rest/agile/1.0/issue/~a" base-url issue-id))
         (basic-auth `(,(jira-user jira) . ,(jira-password jira)))
         (resp (dex:get url
                        :headers '(("Accept" . "application/json"))
                        :basic-auth basic-auth))
         (resp (jzon:parse resp)))
    (issue-from-jira-response jira resp)))

(defmethod assigned-issues ((jira jira) &key (sprint nil))
  "List JIRA issues assigned to user.
Filter issues only for SPRINT if provided."
  (with-accessors ((base-url jira-url)
                   (user jira-user)
                   (pass jira-password)) jira
    (format nil "ass = boobs ~@[AND sprint =~a~]" "ll")
    (let* ((query (quri:url-encode (format nil "assignee = currentuser() ~@[AND sprint=\"~a\"~] AND status != \"Done\"" sprint)))
           (url (format nil "~a/rest/api/2/search?jql=~a" base-url query))
           (basic-auth `(,user . ,pass))
           (resp (dex:get url
                          :headers '(("Accept" . "application/json"))
                          :basic-auth basic-auth))
           (resp (jzon:parse resp)))
      (map 'list
           (op (issue-from-jira-response jira _))
           (@ resp "issues")))))
;;---

(defclass issue ()
  ((id :initarg :id :accessor issue-id)
   (title :initarg :title  :accessor issue-title)
   (description :initarg :description :accessor issue-description)
   (url :initarg :url :accessor issue-url))
  (:documentation "A single issue."))

(defun issue-from-jira-response (jira resp)
  "Create a new ISSUE from JIRA response RESP."
  (make-instance
   'issue
   :id (@ resp "key")
   :title (@ (@ resp "fields") "summary")
   :description (@ (@  resp "fields") "description")
   :url (format nil "~a/browse/~a" (jira-url jira) (@ resp "key"))))

(defmethod to-plist ((issue issue))
  `(:id ,(issue-id issue)
    :title ,(issue-title issue)
    :description ,(issue-description issue)
    :url ,(issue-url issue)))
