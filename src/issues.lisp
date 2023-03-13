(in-package #:saunf)

(defsection @managing-issues (:title "managing Issues")
  "An issue is a request for a new feature, a bug report etc. For example,
a Jira or Github issue. Saunf provides this abstraction to allow working with
different issue management solutions with a single interface."
  "## API"
  (issue-manager class)

  (jira class))

(defclass issue-manager ()
  ((name :initarg :name
         :initform (error "Issue manager name is required")))
  (:documentation "Parent class for all issue managers."))

(defclass jira (issue-manager)
  ((user :initarg :user
         :initform (error "Jira username is required")
         :documentation "email used for authenticating with Jira"))
  (:documentation "Interact with Jira projects."))

(defun fetch-jira-issue (issue-id &key basic-auth)
  (let* ((base-url "https://onrunning.atlassian.net")
         (url (format nil "~a/rest/agile/1.0/issue/~a" base-url issue-id))
         (resp (dex:get url
                        :headers '(("Accept" . "application/json"))
                        :basic-auth basic-auth))
         (resp (jzon:parse resp))
         (issue (make-instance
                 'issue
                 :id (@ resp "key")
                 :title (@ (@ resp "fields") "summary")
                 :description (@ (@  resp "fields") "description")
                 :url (format nil "~a/browse/~a" base-url issue-id))))
    (issue-plist issue)))

(defclass issue ()
  ((id :initarg :id :accessor issue-id)
   (title :initarg :title  :accessor issue-title)
   (description :initarg :description :accessor issue-description)
   (url :initarg :url :accessor issue-url)))

(defun issue-plist (issue)
  `(:id ,(issue-id issue)
    :title ,(issue-title issue)
    :description ,(issue-description issue)
    :url ,(issue-url issue)))
