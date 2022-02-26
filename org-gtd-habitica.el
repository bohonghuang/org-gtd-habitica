;;; org-gtd-habitica.el --- Integration of `org-gtd' and `habitica' -*- lexical-binding: t -*-

;;; Commentary:
;;; This package create an integrated workflow for `org-gtd' and `habitica'.

(require 'habitica)
(require 'org-gtd)

(defconst org-gtd-habitica-buffer-name "*habitica*")
(defconst org-gtd-habitica-stats-name "Stats")
(defconst org-gtd-habitica-habits-name "Habits")
(defconst org-gtd-habitica-daily-tasks-name "Daily Tasks")
(defconst org-gtd-habitica-todos-name "To-Dos")
(defconst org-gtd-habitica-rewards-name "Rewards")

(defun org-gtd-habitica-refresh ()
  (save-window-excursion (habitica-tasks)))

(defmacro save-point-excursion (&rest body)
  "Assuming any text change in `BODY' occurs after the initial position of point."
  `(save-window-excursion
     (let ((original-point (point))
           (original-buffer (current-buffer)))
       ,@body
       (with-current-buffer original-buffer
         (goto-char original-point)))))

(defmacro with-habitica-buffer (&rest body)
  `(let ((buffer-or-name org-gtd-habitica-buffer-name))
     (when (null (get-buffer buffer-or-name))
       (org-gtd-habitica-refresh))
     (with-current-buffer buffer-or-name
       ,@body)))

(defmacro with-habitica-type (type-name &rest body)
  `(with-habitica-buffer
    (let (ret)
      (org-map-entries
       (lambda ()
         (let ((current-type-name (nth 4 (org-heading-components))))
           (setq ret (or ret (when (string-equal current-type-name ,type-name) ,@body)))))
       "LEVEL=1")
      ret)))

(defmacro with-habitica-task (type-name task-name &rest body)
  `(with-habitica-type
    ,type-name
    (let (ret)
      (org-map-entries
       (lambda ()
         (let ((current-task-name (nth 4 (org-heading-components))))
           (setq ret (or ret (when (string-equal current-task-name ,task-name) ,@body)))))
       "LEVEL=2"
       'tree)
      ret)))

(defun org-gtd-habitica-gtd-id-marker-alist ()
  (org-map-entries
   (lambda ()
     (cons (org-entry-get (point) "HABITICA_ID") (point-marker)))
   (concat "HABITICA_ID={.+}")
   (list (org-gtd--path org-gtd-default-file-name))))

(defun org-gtd-habitica-set-todo-state (arg)
  (let ((todo-state (pcase arg ((pred booleanp) (if arg "DONE" "TODO")) ((pred stringp) arg))))
    (save-excursion
    (org-back-to-heading)
    (let* ((has-todo-state (progn (search-forward-regexp  "\\*+ ") (looking-at-p org-todo-regexp)))
           (end (progn (when has-todo-state (search-forward-regexp org-todo-regexp (line-end-position))) (point)))
           (beg (progn (when has-todo-state (search-backward-regexp org-todo-regexp (line-beginning-position))) (point))))
      (delete-region beg end)
      (insert todo-state)
      (unless has-todo-state (insert " "))))))

(defun org-gtd-habitica-sync-task (&optional gtd-id-marker-alist)
  (pcase-let*
      ((gtd-id-marker-alist (or gtd-id-marker-alist (org-gtd-habitica-gtd-id-marker-alist)))
       (task-command)
       (task-status (nth 2 (org-heading-components)))
       (habitica-id (org-entry-get (point) "HABITICA_ID"))
       (heading-and-properties-beg (point))
       (heading-and-properties-end (progn (org-end-of-meta-data t) (point)))
       (heading-and-properties (buffer-substring-no-properties heading-and-properties-beg heading-and-properties-end))
       (has-repeat (org-get-repeat))
       (has-subtasks (org-at-item-p))
       (gtd-heading-marker (cdr (assoc habitica-id gtd-id-marker-alist)))
       (gtd-heading-level)
       (gtd-type (pcase (cons has-repeat has-subtasks)
                   (`(t . ,_) org-gtd-calendar)
                   (`(nil . nil) org-gtd-actions)
                   (`(nil . t) org-gtd-projects)))
       (subtask-text (and has-subtasks (org-list-to-subtree (org-list-to-lisp) (1+ (org-outline-level)))))
       (`(,subtask-text . ,subtask-list)
        (if has-subtasks
            (with-temp-buffer
              (org-mode)
              (insert subtask-text)
              (let ((list (org-map-entries
                           (lambda ()
                             (cons (nth 4 (org-heading-components)) (org-entry-is-done-p))))))
                (cons (buffer-string) list)))
          '(nil . nil)))
       (subtask-indices (and has-subtasks (--map-indexed (cons (car it) it-index) subtask-list)))
       (subtask-commands))
    (when (and has-subtasks (eq gtd-type org-gtd-actions)) (setq gtd-type org-gtd-projects))
    (if gtd-heading-marker
        (progn
          (when has-subtasks       ; 从 Habitica 同步子任务到 GTD 中
            (with-current-buffer (marker-buffer gtd-heading-marker)
              (goto-char gtd-heading-marker)
              (setq gtd-heading-level (org-outline-level))
              (save-restriction
                (org-narrow-to-subtree)
                (org-end-of-meta-data t)
                (set-mark (point-max))
                (org-map-entries    ; 映射 GTD 的子任务
                 (lambda ()
                   (let* ((task-name (nth 4 (org-heading-components)))
                          (task (assoc-string task-name  subtask-list))
                          (done-p (car (org-entry-is-done-p))))
                     (if task
                         (pcase-let ((`(,_ . ,status) task))
                           (pcase `(,done-p . ,(not (null status)))
                             (`("DONE" . nil) (push `(done . ,task-name) subtask-commands)) ; 更改 Habitica 子任务状态为完成
                             (`("CNCL" . ,_) (push `(delete . ,task-name) subtask-commands)) ; 更改 Habitica 子任务状态为完成
                             (`(nil . t) (org-gtd-habitica-set-todo-state t))) ; 更改 GTD 子任务状态为完成
                           (setf subtask-list (assoc-delete-all task-name subtask-list)))
                       (pcase done-p
                         ("DONE" (push `(new-done . ,task-name) subtask-commands))
                         ("CNCL")
                         (_ (push `(new-done . ,task-name) subtask-commands)))))) ; 向 GTD 插入子任务
                 t
                 'region)
                (goto-char (point-max))
                (pcase-dolist (`(,task-name . ,task-status) subtask-list)
                  (org-insert-heading)
                  (insert task-name)
                  (while (<= (org-outline-level) gtd-heading-level) (org-demote))
                  (org-gtd-habitica-set-todo-state (not (null task-status)))))))
          (setq task-command (pcase (car (with-current-buffer (marker-buffer gtd-heading-marker) (goto-char gtd-heading-marker) (org-entry-is-done-p))) ; GTD 向 Habitica 同步
                               ("DONE" 'done)
                               ("CNCL" 'delete))))
      (with-temp-buffer                                       ; 为 GTD 创建任务
        (org-mode)
        (insert heading-and-properties)
        (when has-subtasks (insert subtask-text))
        (org-gtd-habitica-set-todo-state "NEXT")
        (with-org-gtd-refile gtd-type
          (org-refile 3 nil (car (org-refile-get-targets))))))
    (org-narrow-to-subtree)
    (pcase-dolist (`(,command . ,task-name) subtask-commands)
      (save-excursion
        (pcase command
          ('done                        ; 完成 Habitica 子任务
           (forward-line (cdr (assoc-string task-name subtask-indices)))
           (habitica-score-checklist-item))
          ('new                         ; 新建 Habitica 子任务
           (goto-char (point-max))
           (habitica-add-item-to-checklist task-name))
          ('new-done
           (goto-char (point-max))
           (habitica-add-item-to-checklist task-name)
           (habitica-score-checklist-item))
          ('delete
           (forward-line (cdr (assoc-string task-name subtask-indices)))
           (habitica-delete-item-from-checklist)))))
    (widen)
    (goto-char heading-and-properties-beg)
    (pcase task-command
      ('done (habitica-up-task))
      ('delete (habitica-delete-task)))
    habitica-id))

(defun org-gtd-habitica-sync ()
  (interactive)
  (org-gtd-habitica-refresh)
  (with-habitica-buffer
   (let ((gtd-id-marker-alist (org-gtd-habitica-gtd-id-marker-alist)))
     (dolist (habitica-type '("todo" "daily"))
       (dolist (habitica-id (org-map-entries
                             (apply-partially #'org-gtd-habitica-sync-task gtd-id-marker-alist)
                             (concat "HABITICA_TYPE=\"" habitica-type "\"")))
         (setf gtd-id-marker-alist (assoc-delete-all habitica-id gtd-id-marker-alist))))
     (pcase-dolist (`(,_ . ,marker) gtd-id-marker-alist) ; Habitica 向 GTD 同步（不存在的任务视为完成）
       (with-current-buffer (marker-buffer marker)
         (save-excursion
           (goto-char marker)
           (unless (org-entry-is-done-p)
             (org-gtd-habitica-set-todo-state t)))))))
  (org-map-entries
   #'org-gtd-habitica-migrate
   "HABITICA_TASK"
   (list (org-gtd--path org-gtd-default-file-name))))

(defun org-gtd-habitica-migrate ()
  (interactive nil org-mode)
  (if (org-get-repeat)
      (org-gtd-habitica-daily-new)
    (org-gtd-habitica-todo-new))
  (org-back-to-heading)
  (org-set-tags (remove "HABITICA_TASK" (org-get-tags))))

(defun org-gtd-habitica-daily-new ()
  (let ((task-name (nth 4 (org-heading-components)))
        (habitica-id))
    (with-habitica-type
     org-gtd-habitica-daily-tasks-name
     (habitica-new-task task-name))
    (org-gtd-habitica-refresh)
    (setq habitica-id
          (with-habitica-task
           org-gtd-habitica-daily-tasks-name
           task-name
           (org-entry-get (point) "HABITICA_ID")))
    (org-set-property "HABITICA_ID" habitica-id)
    (message "Please set timestamp and repeat method for this task manually.")))

(defun org-gtd-habitica-todo-new ()
  (let ((task-name (nth 4 (org-heading-components)))
        (habitica-id))
    (with-habitica-type
     org-gtd-habitica-todos-name
     (habitica-new-task task-name))
    (org-gtd-habitica-refresh)
    (setq habitica-id
          (with-habitica-task
           org-gtd-habitica-todos-name
           task-name
           (org-entry-get (point) "HABITICA_ID")))
    (org-set-property "HABITICA_ID" habitica-id)
    (org-map-entries                    ; 添加子任务
     (lambda ()
       (save-window-excursion
         (let ((subtask-name (nth 4 (org-heading-components))))
           (with-habitica-task
            org-gtd-habitica-todos-name
            task-name
            (habitica-add-item-to-checklist subtask-name)))))
     (concat "LEVEL=" (number-to-string (1+ (org-outline-level))))
     'tree)))

(defun org-gtd-habitica-before-org-gtd--refile (type &rest _)
  (condition-case nil
      (pcase type
        ((pred (lambda (x) (or (string-equal org-gtd-actions x) (string-equal org-gtd-projects x))))
         (org-gtd-habitica-todo-new))
        ((pred (lambda (x) (string-equal org-gtd-calendar x)))
         (if (org-get-repeat)
             (org-gtd-habitica-daily-new)
           (org-gtd-habitica-todo-new)
           (message "Please set timestamp for this task manually.")))
        (type (error (concat "Unknown GTD type: " type))))
    (error
     (org-back-to-heading)
     (org-set-tags (append (org-get-tags) "HABITICA_TASK")))))

(advice-add #'org-gtd--refile :before #'org-gtd-habitica-before-org-gtd--refile)

(defun org-gtd-habitica-after-todo-state-change ()
  (pcase (save-excursion
           (while (> (org-outline-level) 1) (org-up-heading))
           (org-entry-get (point) "ORG_GTD"))    ;; (string-equal org-state "DONE")
    ((and "Projects"
          (let habitica-id (save-excursion (org-up-heading) (org-entry-get (point) "HABITICA_ID")))
          (guard habitica-id))
     (with-habitica-buffer
      (org-map-entries
       #'org-gtd-habitica-sync-task
       (concat "HABITICA_ID=\"" habitica-id "\""))))
    ((and (guard (string-equal org-state "CNCL"))
          (let habitica-id (org-entry-get (point) "HABITICA_ID"))
          (guard habitica-id))
     (with-habitica-buffer
      (org-map-entries
       #'habitica-delete-task
       (concat "HABITICA_ID=\"" habitica-id "\""))
      t))
    (_ (save-window-excursion (habitica-task-done-up)))))

(add-hook 'org-after-todo-state-change-hook #'org-gtd-habitica-after-todo-state-change)

(defun org-gtd-habitica-before-org-todo (&rest _)
  (remove-hook #'org-after-todo-state-change-hook #'habitica-task-done-up))

(advice-add #'org-todo :before #'org-gtd-habitica-before-org-todo)

(provide 'org-gtd-habitica)
;;; org-gtd-habitica.el ends here
