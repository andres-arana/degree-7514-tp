;"Solves the n-queens problem on a nxn board"
(defun queens (n &optional (queen-pos (init-queen-pos n)) (available-pos (init-available-pos n queen-pos)))
  (cond
    ((eq (length queen-pos) n) (mapcar 'car queen-pos))
    ((< (length queen-pos) (caaar queen-pos)) (reset-board n (pop-until-new-selection queen-pos)))
    ((null available-pos) (reset-board n (pop-until-new-selection queen-pos)))
    (t (queens n (cons (car available-pos) queen-pos) (remove-checked (list (caar available-pos)) available-pos)))))

;"Resets the board, making a brand new board and removing the positions in
;check by those in a list of queen positions"
(defun reset-board (n new-queen-pos)
  (queens n new-queen-pos (remove-checked (mapcar 'car new-queen-pos) (make-board n))))

;"Pops elements from the front of a queen position list until finding one
;which has alternative selections. Then it selects the new alternative."
(defun pop-until-new-selection (queen-pos)
  (if (< (length (car queen-pos)) 2)
    (pop-until-new-selection (cdr queen-pos))
    (cons (cdar queen-pos) (cdr queen-pos))))

;"Creates the initial state for the queen positions on a nxn board"
(defun init-queen-pos (n) 
  (list (car (make-board n))))

;"Creates the initial state for the available positions on a nxn board"
(defun init-available-pos (n queen-pos)
  (remove-checked (list (caar queen-pos)) (make-board n)))

;"Removes from board those positions which are checked by queens in a list of
;queen positions"
(defun remove-checked (queen-pos board)
  (cond
    ((null board) nil)
    ((is-queen-in-row queen-pos (car board)) (remove-checked queen-pos (cdr board)))
    (t (let ((filtered-row (remove-checked-in-row queen-pos (car board))))
         (if (null filtered-row)
           (remove-checked queen-pos (cdr board))
           (cons filtered-row (remove-checked queen-pos (cdr board))))))))

;"Checks if a queen is in the same row as the one given"
(defun is-queen-in-row (queen-pos row)
  (some (lambda (x) (eq (caar row) (car x))) queen-pos))

;"Removes from a board row those positions which are checked by queens in a
;list of queen positions"
(defun remove-checked-in-row (queen-pos row)
  (remove-if (lambda (x) (is-position-checked x queen-pos)) row))

;"Checks is a given position is in check by any of the queens located in queen-pos"
(defun is-position-checked (pos queen-pos)
  (cond
    ((null queen-pos) nil)
    ((or (is-queen-check (car queen-pos) pos) (is-position-checked pos (cdr queen-pos))))))

;"Checks if a given position is in check by a queen"
(defun is-queen-check (queen pos)
  (or (is-col-check queen pos) (is-diag-check queen pos) (is-reverse-diag-check queen pos)))

;"Checks if a given position is in column check by a queen"
(defun is-col-check (queen pos)
  (eq (cadr queen) (cadr pos)))

;"Checks if a given position is in a diagonal check by a queen"
(defun is-diag-check (queen pos)
  (cond
    ((and (eq (car queen) (car pos)) (eq (cadr queen) (cadr pos))) t)
    ((or (< (car pos) (car queen)) (< (cadr pos) (cadr queen))) nil)
    (t (is-diag-check queen (list (- (car pos) 1) (- (cadr pos) 1))))))

;"Checks if a given position is in a reverse diagonal check by a queen"
(defun is-reverse-diag-check (queen pos)
  (cond
    ((and (eq (car queen) (car pos)) (eq (cadr queen) (cadr pos))) t)
    ((or (< (car pos) (car queen)) (> (cadr pos) (cadr queen))) nil)
    (t (is-reverse-diag-check queen (list (- (car pos) 1) (+ (cadr pos) 1))))))

;"Creates a nxn board"
(defun make-board (n &optional (row 0))
  (if (eq row n)
    nil
    (cons (make-board-row n row) (make-board n (+ row 1)))))

;"Creates a single row of an nxn board"
(defun make-board-row (n row &optional (col 0))
  (if (eq col n)
    nil
    (cons (list (+ 1 row) (+ 1 col)) (make-board-row n row (+ col 1)))))
