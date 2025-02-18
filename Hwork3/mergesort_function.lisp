(defun partition (lst)
  "Splits a list into two nearly equal halves."
  (labels ((split (lst left right)
             (cond ((null lst) (list (reverse left) (reverse right))) ; If list is empty, return reversed left and right
                   ((null (cdr lst)) (list (reverse (cons (car lst) left)) (reverse right))) ; If one element left, add to left
                   (t (split (cddr lst) (cons (car lst) left) (cons (cadr lst) right)))))) ; Recursively split, alternating elements
    (split lst '() '()))) ; Initial call with empty left and right lists

(defun my-merge (lst1 lst2)
  "Merges two sorted lists into one sorted list."
  (cond ((null lst1) lst2) ; If lst1 is empty, return lst2
        ((null lst2) lst1) ; If lst1 is empty, return lst1
        ((< (car lst1) (car lst2)) (cons (car lst1) (my-merge (cdr lst1) lst2))) ; If first of lst1 is smaller, keep it and merge rest
        (t (cons (car lst2) (my-merge lst1 (cdr lst2))))))

(defun mergesort (lst)
  "Implements the recursive mergesort algorithm."
  (if (or (null lst) (null (cdr lst))) 
      lst ; If list is empty or has one element, it's already sorted
      (let* ((halves (partition lst))
             (left (car halves))
             (right (cadr halves)))
        (my-merge (mergesort left) (mergesort right)))))
