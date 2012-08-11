(defun seedoc (f)
  "Checks the documentation for a given function"
  (documentation f 'function))

(defun run (program input &optional (memory nil))
  "Executes a C-like program expression

  Params:
    program:
      C-like expression to run.

      Examples:
        ((int x = 0) (main ((x += 1) (scanf y) (printf (x + y)))))

    input:
      Input values to be used for scanf calls. Successive scanf calls in the
      program consume successive values from this list.

      Examples:
        (1 2 3)

    memory:
      Memory map, mapping variable names to values.

      Examples:
        ((x 10) (y 20))

  Returns:
    The list of output values resulting of printf calls in the program

    Examples:
      (2)"
  (cond
    ((null program) nil)
    ((is-var-declaration (car program)) (run (cdr program) input (declare-vars (cdar program) memory)))
    (t (execute-main (cadar program) input memory))))

(defun is-var-declaration (statement)
  "Checks if a statement is a variable declaration

  Params:
    statement:
      Statement to check

      Examples:
        (int x = 0)

  Returns:
    t if the statement is a variable declaration, nil otherwise

    Examples:
      t"
  (equal (car statement) 'int))

(defun declare-vars (statement memory)
  "Modifies a memory map according to a variable declaration statement

  Params:
    statement:
      Variable declaration statement, without the 'int keyword

      Examples:
        (x = 5 y = 10 z = 20)

    memory:
      Memory map, mapping variable names to values.

      Examples:
        ((a 10) (b 20))

  Returns:
    The modified memory map

    Examples:
      ((x 5) (y 10) (z 20) (a 10) (b 20))"
  (cond
    ((null statement) memory)
    ((eq (cadr statement) '=) (declare-vars (cdddr statement) (push-memory memory (car statement) (caddr statement))))
    (t (declare-vars (cdr statement) (push-memory memory (car statement))))))

(defun push-memory (memory name &optional (value 0))
  "Push a name value pair to a memory map

  Params:
    memory:
      Memory map, mapping variable names to values.

      Examples:
        ((a 10) (b 20))

    name:
      Name of the entry to push

      Examples:
        x

    value:
      Optional value of the entry to push. Defaults to 0

      Examples:
        5

  Returns:
    The modified memory map

    Examples:
      ((x 5) (a 10) (b 20))"
  (cons (list name value) memory))

(defun assign-memory (memory name value)
  "Assigns the value of a memory entry

  Params:
    memory:
      Memory map, mapping variable names to values.

      Examples:
        ((a 10) (b 20))

    name:
      Name of the entry to assign

      Examples:
        a

    value:
      Value to assign to the variable

      Examples:
        15

  Returns:
    The modified memory map.
    
    Examples:
      ((a 15) (b 20))"
  (cond
    ((null memory) nil)
    ((eq (caar memory) name) (cons (list name value) (cdr memory)))
    (t (cons (car memory) (assign-memory (cdr memory) name value)))))

(defun in-memory-value (memory name)
  "Obtains the value of a given memory entry

  Params:
    memory:
      Memory map, mapping variable names to values.

      Examples:
        ((a 10) (b 20))

    name:
      Name of the entry to find

      Examples:
        a

  Returns:
    The value in the memory map for the given entry.

    Examples:
      10"
  (cadr (find-if (lambda (x) (equal (car x) name)) memory)))

(defun execute-main (program input memory &optional (output nil))
  "Executes the code inside the main function of a C-like program

  Params:
    program:
      List of statements inside the main function

      Examples:
        ((x = 1) (printf x))

    input:
      Input values to be used for scanf calls. Successive scanf calls in the
      program consume successive values from this list.

      Examples:
        (1 2 3)

    memory:
      Memory map, mapping variable names to values.

      Examples:
        ((x 10) (y 20))

  Returns:
    The list of values associated to printf calls inside the program

    Examples:
      (1)"
  (cond
    ((null program) (reverse output))
    ((eq (caar program) 'printf) (handle-printf-statement program input memory output))
    ((eq (caar program) 'scanf) (handle-scanf-statement program input memory output))))

(defun handle-printf-statement (program input memory output)
  "Special case of the execute-main function which works when the current
  statement is a printf statement"
  (execute-main (cdr program) input memory (cons (evaluate-expression (cadar program) memory) output)))

(defun handle-scanf-statement (program input memory output)
  "Special case of the execute-main function which works when the current
  statement is a scanf statement"
  (execute-main (cdr program) (cdr input) (assign-memory memory (cadar program) (car input)) output))

(defun evaluate-expression (expression memory &optional (operators nil) (operands nil))
  "Evaluates a C-like expression

  Params:
    expression:
      The expression to evaluate.

      Examples:
        (a + b * 3)

    memory:
      Memory map, mapping variable names to values.

      Examples:
        ((a 2) (b 5))

    operators:
      Optional list of accumulated operators. Used by the function to evaluate
      the expressions by accumulating the operators as they are read recursively
      from the expression.

      Examples:
        (+ *)

    operands:
      Optional list of accumulated operands. Used by the function to evaluate
      the expression by accumulating the operands as they are read recursively
      from the expression.

      Examples:
        (a b 3)

  Returns:
    The value of the expression.

    Examples:
      17"
  (cond
    ((null expression) (handle-end-of-expression expression memory operators operands))
    ((atom expression) (handle-atom-expression expression memory))
    (t (handle-complex-expression expression memory operators operands))))

(defun handle-atom-expression (expression memory)
  "Special case of the evaluate-expression function when the expression is an
  atom, either a variable or a number"
  (if (numberp expression)
    expression
    (in-memory-value memory expression)))

(defun handle-end-of-expression (expression memory operators operands)
  "Special case of the evaluate-expression function when the expression has
  been completely parsed"
  (if (null operators)
    (car operands)
    (evaluate-expression expression memory (cdr operators) (pop-operator operators operands))))

(defun handle-complex-expression (expression memory operators operands)
  "Special case of the evaluate-expression function when the expression is a
  complex expression which has not been parsed entirely"
  (if (is-operator (car expression))
    (if (null operators)
      (evaluate-expression (cdr expression) memory (cons (car expression) operators) operands)
      (if (< (operator-weight (car operators)) (operator-weight (car expression)))
        (evaluate-expression (cdr expression) memory (cons (car expression) operators) operands)
        (evaluate-expression expression memory (cdr operators) (pop-operator operators operands))))
    (evaluate-expression (cdr expression) memory operators (cons (evaluate-expression (car expression) memory) operands))))

(defun pop-operator (operators operands)
  "Special case of the evaluate-expression function when the partially parsed
  expression operators need to be popped"
  (cons (translate-booleans (apply (car operators) (list (cadr operands) (car operands)))) (cddr operands)))

(defun translate-booleans (value) 
  "Translates a LISP boolean value to its C counterpart
  
  Params:
    value:
      The value to translate
      
  Returns:
    1 if the value is t, 0 if its nil, the original value if it is a number"
  (if (numberp value)
    value
    (if (null value)
      0
      1)))

(defun is-operator (token)
  "Checks to see if an individual token is an operator

  Params:
    token:
      The token to check

  Returns:
    t if the token is an operator, false otherwise."
  (some (lambda (x) (eq x token)) '(+ - * / < > >= <= ==)))

(defun operator-weight (operator) 
  "Calculates the operator weight of an individual operator. Greater weight
  triggers evaluations when a lower weight operator is found"
  (cond
    ((eq operator '<) 1)
    ((eq operator '>) 1)
    ((eq operator '<=) 1)
    ((eq operator '>=) 1)
    ((eq operator '==) 1)
    ((eq operator '+) 2)
    ((eq operator '-) 2)
    ((eq operator '*) 3)
    ((eq operator '/) 3)))
