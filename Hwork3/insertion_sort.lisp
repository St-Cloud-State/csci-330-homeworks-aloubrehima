(defun insert (item sorted)
  "Inserts an item into the correct position in a sorted list."
  (cond ((null sorted) (list item)) ; If the sorted list is empty, return a list with just the item
        ((<= item (car sorted)) (cons item sorted)) ; If item is less than or equal to the first element, insert at the beginning
        (t (cons (car sorted) (insert item (cdr sorted)))))) ; Otherwise, keep the first element and recursively insert into the rest

(defun insertion-sort (lst)
  "Sorts a list using insertion sort."
  (if (null lst)
      nil ; If the list is empty, return nil
      (insert (car lst) (insertion-sort (cdr lst))))) 

