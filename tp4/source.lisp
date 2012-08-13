(defun seedoc (f)
  "Checks the documentation for a given function"
  (documentation f 'function))

(defun queens (n &optional (queen-pos (init-queen-pos n)) (available-pos (init-available-pos n queen-pos)))
  "Solves the n-queens problem on a nxn board"
  (cond
    ((eq (length queen-pos) n) (mapcar 'car queen-pos))
    ((< (length queen-pos) (caaar queen-pos)) (reset-board n (pop-until-new-selection queen-pos)))
    ((null available-pos) (reset-board n (pop-until-new-selection queen-pos)))
    (t (queens n (cons (car available-pos) queen-pos) (remove-checked (list (caar available-pos)) available-pos)))))

(defun reset-board (n new-queen-pos)
  "Resets the board, making a brand new board and removing the positions in
  check by those in a list of queen positions"
  (queens n new-queen-pos (remove-checked (mapcar 'car new-queen-pos) (make-board n))))

(defun pop-until-new-selection (queen-pos)
  "Pops elements from the front of a queen position list until finding one
  which has alternative selections. Then it selects the new alternative."
  (if (< (length (car queen-pos)) 2)
    (pop-until-new-selection (cdr queen-pos))
    (cons (cdar queen-pos) (cdr queen-pos))))

(defun init-queen-pos (n) 
  "Creates the initial state for the queen positions on a nxn board"
  (list (car (make-board n))))

(defun init-available-pos (n queen-pos)
  "Creates the initial state for the available positions on a nxn board"
  (remove-checked (list (caar queen-pos)) (make-board n)))

(defun remove-checked (queen-pos board)
  "Removes from board those positions which are checked by queens in a list of
  queen positions"
  (cond
    ((null board) nil)
    ((is-queen-in-row queen-pos (car board)) (remove-checked queen-pos (cdr board)))
    (t (let ((filtered-row (remove-checked-in-row queen-pos (car board))))
         (if (null filtered-row)
           (remove-checked queen-pos (cdr board))
           (cons filtered-row (remove-checked queen-pos (cdr board))))))))

(defun is-queen-in-row (queen-pos row)
  "Checks if a queen is in the same row as the one given"
  (some (lambda (x) (eq (caar row) (car x))) queen-pos))

(defun remove-checked-in-row (queen-pos row)
  "Removes from a board row those positions which are checked by queens in a
  list of queen positions"
  (remove-if (lambda (x) (is-position-checked x queen-pos)) row))

(defun is-position-checked (pos queen-pos)
  "Checks is a given position is in check by any of the queens located in queen-pos"
  (cond
    ((null queen-pos) nil)
    ((or (is-queen-check (car queen-pos) pos) (is-position-checked pos (cdr queen-pos))))))

(defun is-queen-check (queen pos)
  "Checks if a given position is in check by a queen"
  (or (is-col-check queen pos) (is-diag-check queen pos) (is-reverse-diag-check queen pos)))

(defun is-col-check (queen pos)
  "Checks if a given position is in column check by a queen"
  (eq (cadr queen) (cadr pos)))

(defun is-diag-check (queen pos)
  "Checks if a given position is in a diagonal check by a queen"
  (cond
    ((and (eq (car queen) (car pos)) (eq (cadr queen) (cadr pos))) t)
    ((or (< (car pos) (car queen)) (< (cadr pos) (cadr queen))) nil)
    (t (is-diag-check queen (list (- (car pos) 1) (- (cadr pos) 1))))))

(defun is-reverse-diag-check (queen pos)
  "Checks if a given position is in a reverse diagonal check by a queen"
  (cond
    ((and (eq (car queen) (car pos)) (eq (cadr queen) (cadr pos))) t)
    ((or (< (car pos) (car queen)) (> (cadr pos) (cadr queen))) nil)
    (t (is-reverse-diag-check queen (list (- (car pos) 1) (+ (cadr pos) 1))))))

(defun make-board (n &optional (row 0))
  "Creates a nxn board"
  (if (eq row n)
    nil
    (cons (make-board-row n row) (make-board n (+ row 1)))))

(defun make-board-row (n row &optional (col 0))
  "Creates a single row of an nxn board"
  (if (eq col n)
    nil
    (cons (list (+ 1 row) (+ 1 col)) (make-board-row n row (+ col 1)))))
