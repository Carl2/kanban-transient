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

(require 'cl-lib)
;;; Commentary:

;; commentary

;;; Code:

(defvar kanban--selected-boards nil
  "List of selected board positions for batch operations.")

(defconst kanban-property-switches
  '(("mirror" . ("--mirrored=" . nil))
    ("match"   . ("--match=" . t)))
  "Alist mapping names to switches.")

(defun kanban-get-value-from-alist (key)
  " Simple function to just get the value from a key"
  (car (cdr (assoc key kanban-property-switches))))
;;(cdr (assoc key kanban-property-switches)))

(defun kanban-is-property-quoted (key)
  "Returns t if the property should be quoted"
  (cdr (cdr (assoc key kanban-property-switches)))
  )


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
  "initialize the kanban board"
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
    (kanban-jump-to-board)
    ("p" "Properties" kanban-properties)]
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




;; This is the indidual kanban boards
;; Property prefix
(defun kanban--get-board-info (pos)
  "Get board info at position POS."
  (save-excursion
    (goto-char pos)
    (when (looking-at "#\\+BEGIN: kanban\\(.*\\)$")
      (let* ((params-str (match-string 1))
             (line (line-number-at-pos))
             (mirrored (if (string-match ":mirrored\\s-+\\(t\\|nil\\)" params-str)
                          (match-string 1 params-str)
                        "nil")))
        (list :pos pos
              :line line
              :params params-str
              :mirrored mirrored)))))


(transient-define-suffix kanban-toggle-board-selection ()
  "Toggle selection of boards for batch operations."
  :description "Select boards"
  :key "m"
  :transient t
  (interactive)
  (let* ((all-boards (mapcar #'kanban--get-board-info (kanban-find-boards)))
         (candidates (mapcar (lambda (board)
                              (let* ((pos (plist-get board :pos))
                                     (checked (member pos kanban--selected-boards)))
                                (cons (kanban--format-board-candidate board checked)
                                      pos)))
                            all-boards))
         (choice (completing-read "Toggle board (TAB to complete): "
                                 (mapcar #'car candidates)
                                 nil t))
         (pos (cdr (assoc choice candidates))))
    (if (member pos kanban--selected-boards)
        (setq kanban--selected-boards (delete pos kanban--selected-boards))
      (push pos kanban--selected-boards))
    (message "Selected %d board(s)" (length kanban--selected-boards))))

(defun kanban--format-board-candidate (board-info checked)
  "Format a board candidate for selection."
  (let ((check (if checked "☑" "☐")))
    (format "%s Board at line %d (mirrored: %s)"
            check
            (plist-get board-info :line)
            (plist-get board-info :mirrored))))


(transient-define-suffix kanban-select-all-boards ()
  "Select all boards."
  :description "Select all"
  :key "M"
  (interactive)
  (setq kanban--selected-boards (kanban-find-boards))
  (message "Selected all %d board(s)" (length kanban--selected-boards)))

(transient-define-suffix kanban-clear-selection ()
  "Clear board selection."
  :description "Clear selection"
  :key "C"
  :transient t
  (interactive)
  (setq kanban--selected-boards nil)
  (message "Cleared board selection"))


(transient-define-suffix kanban-apply-mirror ()
  "Apply mirrored setting to selected boards."
  :description "Apply mirrored"
  :key "a"
  (interactive)
  (let* ((args (transient-args 'kanban-properties))
         (mirrored-value (transient-arg-value "--mirrored=" args))
         (prop-regex ":mirrored\\s-+\\(?:t\\|nil\\)")
         (boards (or kanban--selected-boards
                    (when (y-or-n-p "No boards selected. Apply to current board?")
                      (list (save-excursion
                             (unless (looking-at "#\\+BEGIN: kanban")
                               (re-search-backward "#\\+BEGIN: kanban" nil t))
                             (point)))))))
    (if boards
        (progn
          (dolist (pos boards)
            (kanban--update-board-property pos "mirrored" (or mirrored-value "nil") prop-regex))
          (message "Updated mirrored property for %d board(s)" (length boards))
          ;; Update the boards after changing properties
          (kanban-exec-fn-all-blocks #'org-dblock-update))
      (message "No boards to update"))))



(transient-define-suffix kanban-show-selection ()
  "Show currently selected boards."
  :description "Show selection"
  :key "?"
  :transient t
  (interactive)
  (if kanban--selected-boards
      (let ((board-infos (mapcar #'kanban--get-board-info kanban--selected-boards)))
        (message "Selected boards: %s"
                 (mapconcat (lambda (b)
                             (format "Line %d" (plist-get b :line)))
                           board-infos ", ")))
    (message "No boards selected")))

(defun kanban--update-board-property2 (pos fn)
  "Update PROPERTY with VALUE for board at POS."
  (save-excursion
    (goto-char pos)
    (when (looking-at "#\\+BEGIN: kanban\\(.*\\)$")
      (let* ((params (match-string 1))
             (properties (funcall fn params))
             )
        (message "Properties %s %s" properties params)
        (beginning-of-line)
        (kill-line)
        (insert "\n")
        (previous-line)
        (insert (format "#+BEGIN: kanban %s" properties ))))))

(defun kanban-replace-property-fn (name property  new-val)
  "Return a closure that replaces :PROPERTY with NEW-VAL in a plist-like string."
  (let* ((key property)
         (is-quoted (kanban-is-property-quoted name))
         ;TODO: Here we check if its quoted, and either leave it as a string
         ;or make intern.
         (val (if is-quoted
                  new-val
                (intern new-val)
                  )))
    (lambda (prop-arg)
      (let* ((plist (car (read-from-string (concat "(" prop-arg ")"))))
             (updated (plist-put plist key val)))
        (substring (prin1-to-string updated) 1 -1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Apply the property update                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kanban-get-property-fn (property-name args)
  " Create function for updating property fields.
switch transforms --mirrored → :mirrored which is a canonical name.
"
  (let* ((prop-name (kanban-get-value-from-alist property-name))
         (switch (intern (concat ":" (substring prop-name 2 -1))))
         (prop-value (transient-arg-value prop-name args))
         )
    (message "→→→→ %s → %s" prop-name prop-value)
    (kanban-replace-property-fn property-name switch prop-value)
    ))

(defun kanban--get-property-fns (args)
  "Return a list of property functions for each entry in `kanban-property-switches'.
Each element is the result of calling `kanban-get-property-fn' with the
property name (the car of each element of `kanban-property-switches')."
  (mapcar (lambda (entry)
            (kanban-get-property-fn (car entry) args))
          kanban-property-switches))

(defun kanban--update-position-with-fns (pos property-update-fns)
  "Apply each function in PROPERTY-UPDATE-FNS to the board position POS.

POS is a board position (in the form expected by
`kanban--update-board-property2').

PROPERTY-UPDATE-FNS is a list of functions (or function descriptors)
that `kanban--update-board-property2' understands. For each function
FN in PROPERTY-UPDATE-FNS this function calls:

  (kanban--update-board-property2 POS FN)

and collects the results.

Returns a list of the results from each call, in the same order as
PROPERTY-UPDATE-FNS. Any errors raised by the update functions are
propagated to the caller."
  (mapcar (lambda (fn)
            (kanban--update-board-property2 pos fn))
          property-update-fns))


(defun kanban-update-board-property (selected-list args)
  "Update board properties for each position in SELECTED-LIST.

SELECTED-LIST is a list of board position objects. For each position
in the list this function fetches the current property functions via
`kanban--get-property-fns` and applies them by calling
`kanban--update-position-with-fns` with the position and those
functions. This function performs side effects (it updates the
positions) and returns a list of the values returned by
`kanban--update-position-with-fns` for each position."
  (let* ((prop-fns (kanban--get-property-fns args)))
    (mapcar (lambda (board-pos)
              (kanban--update-position-with-fns board-pos prop-fns))
            selected-list)))


(transient-define-suffix  kanban-apply-property-update()
  "Documentation string"
  :description "Apply selected"
  :key "P"                  ;; Key to trigger this suffix
  (interactive )
  (let* ((args (transient-args 'kanban-properties)))
    (kanban-update-board-property kanban--selected-boards args)
    ))



(transient-define-infix kanban-mirror-option ()
  "Set mirrored property value."
  :description "Mirrored"
  :class 'transient-option
  :key "-p"
  :argument "--mirrored="
  :choices '("t" "nil")
  :init-value (lambda (obj) (oset obj value "nil")))

(transient-define-infix kanban-match-option ()
  "Set PROPERTY for a kanban board to MATCH a string
"
  :description "Match"
  :shortarg "-m"
  :argument "--match="
  :always-read t
  :class 'transient-option)


(transient-define-prefix kanban-properties ()
  "Manage kanban board properties."
  [:description
   (lambda ()
     (format "Board Properties (%d selected)"
             (length kanban--selected-boards)))
   ["Selection"
    :class transient-row
    (kanban-toggle-board-selection)
    (kanban-select-all-boards)
    (kanban-clear-selection)
    (kanban-show-selection)]
   ["Properties"
    (kanban-mirror-option)
    (kanban-match-option)]
   ["Actions"
    :class transient-row
    (kanban-apply-mirror)
    (kanban-apply-property-update)
    ("u" "Update boards" kanban-update-boards)
    ("q" "Back" transient-quit-one)]])




(provide 'kanban-transient)

(defun test-prepare ()
  "docstring"
  (interactive )
  )

;;; kanban-transient.el ends here
