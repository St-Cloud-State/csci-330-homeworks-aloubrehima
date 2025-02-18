(defun my-merge (lst1 lst2)
  "Merges two sorted lists into one sorted list."
  (cond ((null lst1) lst2)
        ((null lst2) lst1)
        ((< (car lst1) (car lst2)) (cons (car lst1) (my-merge (cdr lst1) lst2)))
        (t (cons (car lst2) (my-merge lst1 (cdr lst2))))))

(defun pairwise-sort (lst)
  "Partitions the list into sorted pairs."
  (cond ((null lst) nil)
        ((null (cdr lst)) (list lst))
        (t (cons (my-merge (list (car lst)) (list (cadr lst)))
                 (pairwise-sort (cddr lst))))))

(defun merge-pairs (lst)
  "Merges adjacent sorted lists."
  (cond ((null lst) nil)
        ((null (cdr lst)) lst)
        (t (cons (my-merge (car lst) (cadr lst))
                 (merge-pairs (cddr lst))))))

(defun bottom-up-mergesort (lst)
  "Implements bottom-up mergesort."
  (if (null lst)
      nil
      (let ((sorted (pairwise-sort lst)))
        (loop while (> (length sorted) 1) do
          (setf sorted (merge-pairs sorted)))
        (car sorted))))

