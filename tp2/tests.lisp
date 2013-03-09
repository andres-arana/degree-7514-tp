; Evaluate a lambda expression located in the environment
(defun test-evaluate-lambda-in-env () 
  (evaluate '(f 1) '((f (lambda (x) (+ x (* 2 3)))))))

; Evaluate a mapcar expression with a local in the environment
(defun test-evaluate-mapcar-local ()
  (evaluate '(mapcar (lambda (x) (+ x y)) (list 1 2 3)) '((y 2))))

; Evaluate an expression containing if
(defun test-evaluate-if-expression ()
  (evaluate '(mapcar (lambda (x) (if (or (< x 3) (> x 4)) (+ x 10) (+ 100 (* x 10)))) (list 1 2 3 4 5 6)) nil))

