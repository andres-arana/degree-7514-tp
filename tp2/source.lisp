(defun seedoc (f)
  "Checks the documentation for a given function"
  (documentation f 'function))

(defun evaluate (expression environment)
  "Evaluates a LISP expression in a given environment

  Params:
    expression:
      The expression to evaluate

      Examples:
        (+ 1 2)
        (lambda (x) (+ x 1))
        5

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      3"
  (if (atom expression)
    (evaluate-atom expression environment)
    (evaluate-list expression environment)))

(defun evaluate-list (expression environment)
  "Evaluates a LISP list expression in a given environment

  Params:
    expression:
      The expression to evaluate

      Examples:
        (+ 1 2)
        (lambda (x) (+ x 1))

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      3"
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
  "Evaluates a LISP function call expression in a given environment

  Params:
    function-name:
      The name of the function to call, or a lambda expression to call

      Examples:
        car
        (lambda (x) (+ x 1))

    arguments:
      List of evaluated argument values for the function call

      Examples:
        ((1 2 3))
        (5)

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      3"
  (if (atom function-name)
    (if (null (lookup function-name environment))
      (cond
        ((equal function-name 'mapcar) (handle-mapcar-call arguments environment))
        ((equal function-name 'apply) (handle-apply-call arguments environment))
        (t (call-builtin-function function-name arguments)))
      (evaluate-function-call (evaluate function-name environment) arguments environment))
    (evaluate (caddr function-name) (extended-environment (cadr function-name) arguments environment))))

(defun evaluate-argument-list (expression environment)
  "Evaluates a list of arguments in an expression

  Params:
    expression:
      The expression whose arguments are evaluated

      Examples:
        (+ (* 1 2) (* 4 3))

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The list of evaluated arguments

    Examples:
      (2 12)"
  (mapcar (lambda (x) (evaluate x environment)) (cdr expression)))

(defun evaluate-atom (expression environment)
  "Evaluates an atom expression in a given environment

  Params:
    expression:
      The expression to evaluate

      Examples:
        1
        a

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      1
      10"
  (cond
    ((numberp expression) expression)
    ((equal 't expression) t)
    (t (lookup expression environment))))

(defun extended-environment (argument-list argument-values environment)
  "Extends the environment with the values from an argument list

  Params:
    argument-list:
      The list of arguments of a function

      Examples:
        (x y)

    argument-values:
      The list of argument values of a function

      Examples:
        (10 20)

    environment:
      List of environment mappings

      Examples:
        ((a 10))

  Returns:
    The extended environment

    Examples:
      ((a 10) (x 10) (y 20))"
  (if (null argument-list)
    environment
    (cons
      (list (car argument-list) (car argument-values))
      (extended-environment (cdr argument-list) (cdr argument-values) environment))))

(defun lookup (expression environment)
  "Looks up an expression in the environmrent

  Params:
    expression:
      The expression look up

      Examples:
        a

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The environment value for the expression

    Examples:
      10"
  (let ((value (find-if (lambda (x) (equal (car x) expression)) environment)))
    (if (null value)
      nil
      (cadr value))))

; Builtin function handlers
(defun call-builtin-function (function-name arguments)
  "Evaluates a builtin function

  Params:
    expression:
      The function to evaluate

      Examples:
        car
        cons

    arguments:
      The list of evaluated arguments

      Examples:
        ((1 2 3))
        (1 (2 3))

  Returns:
    The result of evaluating the expression

    Examples:
      1
      (1 2 3)"
  (apply function-name arguments))

; Special form handlers
(defun handle-quote (expression environment)
  "Evaluates a quote expression

  Params:
    expression:
      The expression to evaluate

      Examples:
        (quote a)

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      a"
  (cadr expression))

(defun handle-if (expression environment)
  "Evaluates an if expression

  Params:
    expression:
      The expression to evaluate

      Examples:
        (if a 1 2)

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      1"
  (if 
    (evaluate (cadr expression) environment)
    (evaluate (caddr expression) environment)
    (evaluate (cadddr expression) environment)))

(defun handle-cond (expression environment &optional (conditions (cadr expression)))
  "Evaluates a cond expression

  Params:
    expression:
      The expression to evaluate

      Examples:
        (cond (a 1) (t 2))

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      1"
  (let ((current-condition (car conditions)))
    (if (evaluate (car current-condition) environment)
      (evaluate (cadr current-condition) environment)
      (handle-cond expression environment (cdr conditions)))))

(defun handle-and (expression environment &optional (conditions (cdr expression)))
  "Evaluates an and expression

  Params:
    expression:
      The expression to evaluate

      Examples:
        (and (eq a 10) (eq a 20))

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      nil"
  (let ((current-condition (car conditions)))
    (cond
      ((null conditions) t)
      ((evaluate current-condition environment) (handle-and expression environment (cdr conditions)))
      (t nil))))

(defun handle-or (expression environment &optional (conditions (cdr expression)))
  "Evaluates an or expression

  Params:
    expression:
      The expression to evaluate

      Examples:
        (or (eq a 10) (eq a 20))

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      t"
  (let ((current-condition (car conditions)))
    (cond
      ((null conditions) nil)
      ((evaluate current-condition environment) t)
      (t (handle-or expression environment (cdr conditions))))))

(defun handle-lambda (expression environment)
  "Evaluates a lambda expression

  Params:
    expression:
      The expression to evaluate

      Examples:
        (lambda (x) (+ x 1))

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      (lambda (x) (+ x 1))"
  expression)

(defun handle-mapcar-call (arguments environment)
  "Evaluates a native mapcar function call

  Params:
    arguments:
      The arguments to the mapcar call

      Examples:
        ((lambda (x) (+ x 1)) (a 2 3))

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      (11 3 4)"
  (let
    ((function-name (car arguments)) (elements (cadr arguments)))
    (cond
      ((null elements) nil)
      (t (cons
           (evaluate-function-call function-name (list (car elements)) environment)
           (handle-mapcar-call (list function-name (cdr elements)) environment))))))

(defun handle-apply-call (arguments environment)
  "Evaluates an apply native function call

  Params:
    arguments:
      The arguments to the apply call

      Examples:
        ((lambda (x) (+ x 1)) (1))

    environment:
      List of environment mappings

      Examples:
        ((a 10) (f (lambda (x) (+ x 1))))

  Returns:
    The result of evaluating the expression

    Examples:
      2"
  (let
    ((function-name (car arguments)) (call-arguments (cadr arguments)))
    (evaluate-function-call function-name call-arguments environment)))

