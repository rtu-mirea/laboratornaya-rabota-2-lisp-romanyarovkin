(defun insert-at (number lis value)
  (if (= number 0) (cons value lis)
      (cons (car lis) (insert-at (1- number) (cdr lis) value))))

(defun del-by-num (number lis) 
(cond ((null lis) nil) 
((zerop number) (cdr lis)) 
((cons (car lis) (del-by-num (1- number) (cdr lis))))))

(defun postions (needing_elem lis)
  (loop
    for element in lis
    and position from 0
     when (eql element needing_elem)
      collect position))
