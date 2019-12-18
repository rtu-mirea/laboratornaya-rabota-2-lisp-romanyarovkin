(defun DeepDecompress (lst)
(if (zerop (car lst)) nil
(cons (second lst) (DeepDecompress (list (1- (car lst)) (second lst))))))

(defun Decompress (lst)
(cond ((null lst) nil)
((atom (car lst)) (cons (car lst) (Decompress (cdr lst))))
((consp (car lst)) (append (DeepDecompress (car lst)) (Decompress (cdr lst)))
)))

(defun CompareList (val acc)
(if (> acc 1) (list acc val) val))

(defun Accum (val acc lst)
(cond ((null lst) (cons (CompareList val acc) nil))
((eq val (car lst)) (Accum val (1+ acc) (cdr lst)))
(t (cons (CompareList val acc) (Accum (car lst) 1 (cdr lst))))))

(defun Compress (lst)
(cond ((null (cdr lst)) '())
(t (Accum (car lst) 1 (cdr lst)))))
