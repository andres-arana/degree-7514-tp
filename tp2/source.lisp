; Evaluation functions
(defun evaluate (expression environment)
  "Evaluates a LISP quoted expression in a given environment"
  (if (atom expression)
    (evaluate-atom expression environment)
    (evaluate-list expression environment)))

(defun evaluate-list (expression environment)
  "Evaluates a list expression in a given environment"
  (let ((expression-name (car expression)))
    (cond
      ((equal expression-name 'quote) (handle-quote expression environment))
      ((equal expression-name 'if) (handle-if expression environment))
      ((equal expression-name 'unless) (handle-unless expression environment))
      ((equal expression-name 'cond) (handle-cond expression environment))
      ((equal expression-name 'and) (handle-and expression environment))
      ((equal expression-name 'or) (handle-or expression environment))
      ((equal expression-name 'lambda) (handle-lambda expression environment))
      (t (evaluate-function-call expression-name (evaluate-argument-list expression environment) environment)))))

(defun evaluate-function-call (function-name arguments environment)
  "Evaluates a function call expression in a given environment"
  (if (atom function-name)
    (if (null (lookup function-name environment))
      (cond
        ((equal function-name 'mapcar) (handle-mapcar-call arguments environment))
        ((equal function-name 'apply) (handle-apply-call arguments environment))
        (t (call-builtin-function function-name arguments)))
      (evaluate-function-call (evaluate function-name environment) arguments environment))
    (evaluate (caddr function-name) (extended-environment (cadr function-name) arguments environment))))

(defun evaluate-argument-list (expression environment)
  "Evaluates the arguments of a function call expression in a given environment"
  (mapcar (lambda (x) (evaluate x environment)) (cdr expression)))

(defun evaluate-atom (expression environment)
  "Evaluates an atom expression in a given argument"
  (cond
    ((numberp expression) expression)
    ((equal 't expression) t)
    (t (lookup expression environment))))

(defun extended-environment (argument-list argument-values environment)
  "Extends the environment with the values of an argument list"
  (if (null argument-list)
    environment
    (cons 
      (list (car argument-list) (car argument-values))
      (extended-environment (cdr argument-list) (cdr argument-values) environment))))

(defun lookup (expression environment)
  "Looks an expression up in the environment"
  (let ((value (find-if (lambda (x) (equal (car x) expression)) environment)))
    (if (null value)
      nil
      (cadr value))))

; Builtin function handlers
(defun call-builtin-function (function-name arguments)
  "Evaluates a builtin function"
  (apply function-name arguments))

; Special form handlers
(defun handle-quote (expression environment)
  "Handles a QUOTE expression in a given environment"
  (cadr expression))

(defun handle-if (expression environment)
  "Handles an IF expression in a given environment"
  (if (evaluate (cadr expression) environment) (evaluate (caddr expression) environment) (evaluate (cadddr expression) environment)))

(defun handle-unless (expression environment)
  "Handles an UNLESS expression in a given environment"
  (if (not (evaluate (cadr expression) environment)) (evaluate (caddr expression) environment) (evaluate (cadddr expression) environment)))

(defun handle-cond (expression environment &optional (conditions (cadr expression)))
  "Handles a COND expression in a given environment"
  (let ((current-condition (car conditions)))
    (if (evaluate (car current-condition) environment)
      (evaluate (cadr current-condition) environment)
      (handle-cond expression environment (cdr conditions)))))

(defun handle-and (expression environment &optional (conditions (cdr expression)))
  "Handles an AND expression in a given environment"
  (let ((current-condition (car conditions)))
    (cond
      ((null conditions) t)
      ((evaluate current-condition environment) (handle-and expression environment (cdr conditions)))
      (t nil))))

(defun handle-or (expression environment &optional (conditions (cdr expression)))
  "Handles an OR expression in a given environment"
  (let ((current-condition (car conditions)))
    (cond
      ((null conditions) nil)
      ((evaluate current-condition environment) t)
      (t (handle-or expression environment (cdr conditions))))))

(defun handle-lambda (expression environment)
  "Handles a LAMBDA expression in a given environment"
  expression)

(defun handle-mapcar-call (arguments environment)
  "Handles a MAPCAR function call"
  (let
    ((function-name (car arguments)) (elements (cadr arguments)))
    (cond
      ((null elements) nil)
      (t (cons
           (evaluate-function-call function-name (list (car elements)) environment)
           (handle-mapcar-call (list function-name (cdr elements)) environment))))))

(defun handle-apply-call (arguments environment)
  "Handles an APPLY function call"
  (let
    ((function-name (car arguments)) (call-arguments (cadr arguments)))
    (evaluate-function-call function-name call-arguments environment)))

