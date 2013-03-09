(defun test-program ()
  (run '(
         (int x)
         (int y)
         (int z)
         (main (
                (scanf x)
                (scanf y)
                (scanf z)
                (if (z > x * y) 
                  ((while (x > 0) ((printf y) (x -= 1) (y *= x))))
                else
                  ((printf (x * y * z))))

                )))
       '(5 3 10)))
