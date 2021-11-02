;; -*- lexical-binding: t; -*-

(defun saunf--non-empty-string (str)
  "Return nil if STR is empty. STR otherwise"
  (and (not (string= str "")) str))

(defun saunf--get-org-subtree-body-as-md ()
  (save-mark-and-excursion
    (org-mark-subtree)
    (forward-line)
    (org-export-string-as
     (buffer-substring-no-properties
      (region-beginning) (region-end)) 'md t '(:with-toc nil))))

(defun saunf--create-gh-issue (title body repo &optional labels)
  (let* ((labels-cmd (mapconcat (lambda (l) (format "--label %s" l)) labels " "))
         (body-file (make-temp-file "gh-issue-body" nil nil body))
         (command
          (format
           "gh issue create --title \"%s\" --body-file \"%s\" --repo %s %s"
           title body-file repo labels-cmd))
         (issue-id nil))

    ;; FIXME For some reason, two org-entry-put calls below add two property
    ;; drawers in an org-tree with no existing drawer. Calling
    ;; org-insert-property-drawer before executing shell command isn't fixing
    ;; the problem. Need to find a fix for this.
    (org-insert-property-drawer)
    (message "Executing: %s" command)
    (setq issue-id (shell-command-to-string command))
    (org-insert-property-drawer)
    (org-entry-put (point) "GH_ISSUE" issue-id)
    (org-entry-put (point) "GH_LABELS" (mapconcat #'identity labels " "))
    (message "Created GH Issue: %s" issue-id)))

(defun saunf--update-gh-issue (id title body repo &optional labels old-labels)
  "Update existing with ID"
  (let* ((labels-to-add (seq-difference labels old-labels))
         (labels-to-remove (seq-difference old-labels labels))
         (add-labels (mapconcat (lambda (l) (format "--add-label %s" l)) labels-to-add " "))
         (remove-labels (mapconcat (lambda (l) (format "--remove-label %s" l)) labels-to-remove " "))
         (body-file (make-temp-file "gh-issue-body" nil nil body))
         (command
          (format "gh issue edit %s --title \"%s\" --body-file \"%s\" --repo %s %s %s"
                  id title body-file repo add-labels remove-labels)))

    (message "Executing: %s" command)
    (message "Updated issue: %s" (shell-command-to-string command))
    (apply #'org-entry-put-multivalued-property (point) "GH_LABELS" labels)))

(defun saunf-upsert-as-gh-issue ()
  "Upsert org-node under point as a github issue.
Uses `gh' command to interact with github.

- Github cli (accessible as `gh' on PATH) must be installed and
  authorized
- One of the parent org-nodes must have a =GH_REPO= property.
  This will be used as `-R' option of `gh'
- All the valid-tags applicable on the node will be set as issue
  labels. One of the parent nodes must have a =GH_VALID_LABELS=
  property which has space-separate list of valid labels.
- GH_ISSUE property is inserted in the node PROPERTIES, which
  indicates whether a new issue will be created, or GH_ISSUE value
  will be used in `gh issue edit'
- GH_LABELS property is inserted in node PROPERTIES, which is
  used to figure out which labels to add and which to remove
  "
  (interactive)
  (save-excursion
    (let* ((stree (org-entry-properties))
           (gh-repo (org-entry-get-with-inheritance "GH_REPO"))
           (gh-id (saunf--non-empty-string (org-entry-get (point) "GH_ISSUE")))
           (gh-status (saunf--non-empty-string (org-entry-get (point) "GH_STATUS")))
           (todo-p (org-entry-is-todo-p))
           (title (alist-get "ITEM" stree nil nil #'string=))
           (body (saunf--get-org-subtree-body-as-md))
           (valid-labels (split-string (org-entry-get-with-inheritance "GH_VALID_LABELS") " "))
           (old-labels (org-entry-get-multivalued-property (point) "GH_LABELS"))
           (labels (seq-intersection valid-labels (mapcar #'substring-no-properties (org-get-tags)))))
      (when (not gh-repo)
        (error "Missing required key on any parent: GH_REPO"))

      (cond
       ((and (not todo-p) gh-id)
        (setq command (format "gh issue close -R %s" gh-repo))
        (message "Executing: %s" command)
        (message "Closed issue: %s" (shell-command-to-string command))
        (org-entry-put "GH_STATUS" "CLOSED"))

       ((and todo-p (string= gh-status "CLOSED"))
        (setq command (format "gh issue reopen %s" id))
        (message "Executing: %s" command)
        (message "Reopened issue: %s" (shell-command-to-string command))
        (org-entry-put "GH_STATUS" "OPEN")))

      (cond
       ((not gh-id) (saunf--create-gh-issue title body gh-repo labels))
       (gh-id (saunf--update-gh-issue gh-id title body gh-repo labels old-labels))))))

(provide 'saunf)
