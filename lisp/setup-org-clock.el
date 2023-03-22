;;; setup-org.el --- org-mode clock setup / options
;;; Commentary:
;;; Code:
(use-package org-clock
  :straight t
  :config
  ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Change tasks to NEXT when clocking in
  (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
  ;; Separate drawers for clocking and logs
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  ;; Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)

  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

  (defun bh/clock-in-to-next (kw)
    "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
      (cond
       ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
        "NEXT")
       ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
        "TODO"))))

  (defun bh/remove-empty-drawer-on-clock-out ()
    "Remove empty LOGBOOK drawers on clock out."
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at "LOGBOOK" (point)))))

(provide 'setup-org-clock)
;;; setup-org-clock.el ends here
