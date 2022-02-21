;; org-gtd-habitica.el --- Integration of `org-gtd' and `habitica'. -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;    Integration of `org-gtd' and `habitica'.

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

(defun org-gtd-habitica-sync ()
  (interactive nil org-mode)
  (org-gtd-habitica-refresh)
  (with-habitica-buffer
   (pcase-dolist (`(,habitica-type . ,gtd-type)
                  `(("todo" . ,org-gtd-actions)
                    ("daily" . ,org-gtd-calendar)))
     (org-map-entries
      (lambda ()
        (pcase-let*
            ((habitica-id (org-entry-get (point) "HABITICA_ID"))
             (heading-and-properties-beg (point))
             (heading-and-properties-end (progn (org-end-of-meta-data t) (point)))
             (heading-and-properties (buffer-substring-no-properties heading-and-properties-beg heading-and-properties-end))
             (has-subtasks (org-at-item-p))
             (subtask-text (and has-subtasks (org-list-to-subtree (org-list-parse-list) (1+ (org-outline-level)))))
             (`(,subtask-text . ,subtask-list)
              (if has-subtasks
                  (with-temp-buffer
                    (org-mode)
                    (insert subtask-text)
                    (let ((list (org-map-entries
                                 (lambda ()
                                   (org-set-property "HABITICA_ID" "")
                                   (cons (nth 4 (org-heading-components)) (org-entry-is-done-p))))))
                      (cons (buffer-string) list)))
                '(nil . nil)))
             (subtask-indices (and has-subtasks (--map-indexed (cons (car it) it-index) subtask-list)))
             (done)
             (gtd-heading-marker)
             (subtask-commands))
          (when (and has-subtasks (eq gtd-type org-gtd-actions)) (setq gtd-type org-gtd-projects))
          (setq gtd-heading-marker
                (org-map-entries
                 (lambda ()
                   (when (org-entry-is-done-p)
                     (setq done t))
                   (point-marker))
                 (concat "HABITICA_ID=\"" habitica-id "\"")
                 (list (org-gtd--path org-gtd-default-file-name))))
          (when gtd-heading-marker (setq gtd-heading-marker (car gtd-heading-marker)))
          (if gtd-heading-marker
              (when has-subtasks       ; 从 habitica 同步子任务到 GTD 中
                (with-current-buffer (marker-buffer gtd-heading-marker)
                  (goto-char gtd-heading-marker)
                  (save-restriction
                    (org-narrow-to-subtree)
                    (org-end-of-meta-data t)
                    (set-mark (point-max))
                    (org-map-entries
                     (lambda ()
                       (let* ((task-name (nth 4 (org-heading-components)))
                              (task (assoc-string task-name  subtask-list)))
                         (if task
                             (pcase-let ((`(,_ . ,status) task))
                               (pcase `(,(not (null (org-entry-is-done-p))) . ,(not (null status)))
                                 (`(t . nil) (push `(done . ,task-name) subtask-commands)) ; 更改 habitica 子任务状态为完成
                                 (`(nil . t)
                                  (let* ((end (search-forward-regexp "\\(TODO\\)\\|\\(NEXT\\)"))
                                         (beg (- end 4)))
                                    (delete-region beg end)
                                    (insert "DONE")))) ; 更改 GTD 子任务状态为完成
                               (setf subtask-list (assoc-delete-all task-name subtask-list)))
                           (push `(new . ,task-name) subtask-commands)))) ; 向 GTD 插入子任务
                     t
                     'region)
                    (goto-char (point-max))
                    (pcase-dolist (`(,task-name . ,task-status) subtask-list)
                      (org-insert-heading)
                      (insert task-name)
                      (org-set-property "HABITICA_ID" "")
                      (org-todo (if task-status "DONE" "TODO")))))) ; 向 habitica 插入子任务
            (with-temp-buffer
              (org-mode)
              (insert heading-and-properties)
              (when has-subtasks (insert subtask-text))
              (goto-char (point-min))
              (with-org-gtd-refile gtd-type
                (org-refile 3 nil (car (org-refile-get-targets))))))
          (org-narrow-to-subtree)
          (pcase-dolist (`(,command . ,task-name) subtask-commands)
            (save-excursion
              (pcase command
                ('done
                 (forward-line (cdr (assoc-string task-name subtask-indices)))
                 (habitica-score-checklist-item))
                ('new
                 (goto-char (point-max))
                 (habitica-add-item-to-checklist task-name)))))
          (widen)
          (when done (habitica-task-done-up))))
      (concat "HABITICA_TYPE=\"" habitica-type "\""))))
  (org-map-entries
   #'org-gtd-habitica-migrate
   "HABITICA_UNSYNC"
   (list (org-gtd--path org-gtd-default-file-name))))

(defun org-gtd-habitica-migrate ()
  (interactive nil org-mode)
  (if (org-get-repeat)
      (org-gtd-habitica-daily-new)
    (org-gtd-habitica-todo-new))
  (org-back-to-heading)
  (org-set-tags (remove "HABITICA_UNSYNC" (org-get-tags))))

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

(defun org-gtd--refile@before (type &rest _)
  (condition-case nil
      (pcase type
        ((or org-gtd-actions org-gtd-projects) (org-gtd-habitica-todo-new))
        (org-gtd-calendar
         (if (org-get-repeat)
             (org-gtd-habitica-daily-new)
           (org-gtd-habitica-todo-new)
           (message "Please set timestamp for this task manually."))))
    (error
     (org-back-to-heading)
     (org-set-tags (append (org-get-tags) "HABITICA_UNSYNC")))))

(advice-add #'org-gtd--refile :before #'org-gtd--refile@before)

(provide 'org-gtd-habitica)
;;; org-gtd-habitica.el ends here
