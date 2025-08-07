;;; kanban-transient.el --- summary -*- lexical-binding: t -*-

;; Author: Calle Olsen
;; Maintainer: Calle Olsen
;; Version: 0.0.1
;; Package-Requires: (org-kanban)
;; Homepage: https://cocode.se
;; Keywords: transient


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:


(transient-define-suffix kanban-update-boards()
  "Update all boards"
  :description "Update boards"
  :key "u"                  ;; What key to execute
  (interactive )
  (save-excursion
    ;; (let* ((args (transient-args 'kanban-prefix))
    ;;        (value (transient-arg-value "--option=" args))
    ;;        )
      (kanban-exec-fn-all-blocks #'org-dblock-update)
      ))


(transient-define-infix kanban-initialize-options()
  "Either choose beginning,here or the end"
  :description "Where to initialize the board"
  :class 'transient-option
  :argument "--init-place="
  :shortarg "-i"
  :choices '("beginning" "here" "end")
  :init-value (lambda (obj) (oset obj value "end"))
  :always-read t)

(defun kanban-initialize-at-first-heading ()
  "initialize kanban board at the first heading"
  (save-excursion
    (goto-char (point-min))
    (when (org-next-visible-heading 1)
      (org-kanban/initialize)
      )))



(transient-define-suffix kanban-init-exec()
  "initilize the kanban board"
  :key "i"
  :description "Initialize board"
  (interactive)
  (let* ((args (transient-args 'kanban-prefix))
         (where (transient-arg-value "--init-place=" args))
         )

    (pcase where
      ("here" (org-kanban/initialize-here))
      ((or "beg" "beginning") (kanban/initialize-at-first-heading))
      ((or "end" "end") (org-kanban/initialize-at-end))
      (_ (message "Unknown place: %s" where)))))

(defun kanban-search-forward-for-board (pt name fn)
  "search forward for the dynamic board "
  (let* ((board-regexp (format "^#\\+BEGIN: %s" name)))
    (goto-char pt)
    (when (re-search-forward board-regexp nil t)
      (goto-char (match-beginning 0))
      (funcall fn)
      (point))))



(defun kanban-exec-fn-all-blocks (fn &optional pt name)
  "Call FN for all the db blocks, starting from PT (or point-min). If NAME is not set, use 'kanban'."
  (let* ((start (or pt (point-min)))
         (dblock-name (or name "kanban"))
         (found-point (kanban-search-forward-for-board start dblock-name fn)))
    (if found-point
        (progn
          (goto-char found-point)
          (forward-line 1)
          ;; Recursive call: next search starts at (point)
          (1+ (kanban-exec-fn-all-blocks fn (point) dblock-name)))
      0)
    ))

;; This can be further abstracted by using fn as argument instead of (point)
;; Later!!
(defun store-points-fn (list-of-points)
  "Returns another function that stores points"
  (lambda ()
    (setcdr list-of-points (append (cdr list-of-points) (list (point))))
    ))

(defun kanban-find-boards (&optional pt dblock-name)
  "Find all kanban boards and return their location."
  (save-excursion
    (let* ((my-list (list nil))
           (name (or dblock-name "kanban"))
           (store-fn (store-points-fn my-list))
           (start (or pt (point-min)))
         )
    (progn
      (kanban-exec-fn-all-blocks store-fn start)
      (cdr my-list)
    ))))


(defun kanban-jump-to-position ()
  "Interactively jump to a kanban board position."
  (interactive)
  (let* ((positions (kanban-find-boards))            ; e.g. a list of symbols or numbers
         (candidates (mapcar #'prin1-to-string positions))
         (choice (completing-read "Jump to board: " candidates)))
    (goto-char (string-to-number choice))
    ))


(defun kanban-make-choice-list (board-number pos)
  "Create alist with number and position"
  (let ((linenr (line-number-at-pos pos)))
    (cons (format "Board #%d (line %d)" board-number linenr) pos)
    ))

(defun kanban-create-seq (candidates)
  "create a sequence of all candidates"
  (number-sequence 1 (length candidates))
  )

(transient-define-suffix kanban-jump-to-board()
  "Jumps to specified board"
  :description "Jump to a board"
  :key "j"
  :transient t
  (interactive)
  (let* ((positions (kanban-find-boards)) ;Here we get all the position
         (candidates (cl-mapcar #'kanban-make-choice-list (kanban-create-seq positions) positions))
         (choice (completing-read "Jump to board" (mapcar #'car candidates)))
         (pos (cdr (assoc choice candidates)))
         )
    (message "Pos %d" pos)
    (goto-char pos)
    ))


(transient-define-suffix kanban-field-right()
  "Moving a field to the right"
  :description "→"
  :key "<right>"
  :format " %d"
  :transient t
  (interactive)
  (org-kanban//move 'right)
  )

(transient-define-suffix kanban-field-left()
  "Moving a field to the Left"
  :description "←"
  :key "<left>"
  :format " %d"
  :transient t
  (interactive)
  (org-kanban//move 'left)
  )

(transient-define-suffix kanban-field-up()
  "Moving a field to the Left"
  :description "↑"
  :key "<up>"
  :transient t
  :format " %d"
  (interactive)
  (forward-line -1)
  )

(transient-define-suffix kanban-field-down()
  "Moving a field to the Left"
  :description "↓"
  :key "<down>"
  :format " %d"
  :transient t
  (interactive)
  (forward-line 1)
  )


(transient-define-suffix kanban-row-up()
  "move up one row."
  :description "Subtree up"
  :key "w"                  ;; What key to execute
  :transient t
  (interactive)
  (org-kanban//move-subtree 'up)
  )
(transient-define-suffix kanban-row-down()
  "move down one row."
  :description "Subtree dwn"
  :key "s"                  ;; What key to execute
  :transient t
  (interactive)
  (org-kanban//move-subtree 'down)
  )


(transient-define-prefix kanban-prefix()
  "Kanban prefix execution"
  ["Kanban"
   ["Operations"
    (kanban-init-exec)
    (kanban-update-boards)
    (kanban-jump-to-board)]
   ["Subtree"
    (kanban-row-up)
    (kanban-row-down)]
   ["field" :class transient-column
    (kanban-field-left)]
   ["table" (kanban-field-up) (kanban-field-down)]
   ["edit" (kanban-field-right) ]
   ]
  ["Options"
   [(kanban-initialize-options)]
   [("q" "Quit" transient-quit-one :face '(:foreground "red"))]
   ])


(provide 'kanban-transient)

(defun test-prepare ()
  "docstring"
  (interactive )
  )

;;; kanban-transient.el ends here
