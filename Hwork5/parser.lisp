(defvar *input* nil)    ; input string as a list of characters
(defvar *pos* 0)        ; Current position in the input
(defvar *error-msg* nil) ; Q8

;;; Utility Functions
(defun peek ()
  "Peek at the current symbol without consuming it."
  (if (< *pos* (length *input*))
      (nth *pos* *input*)
      nil))

(defun match (expected)
  "Consume the current symbol if it matches the expected one."
  (let ((current (peek)))
    (if (and current (equal current expected))
        (progn (incf *pos*) t)
        (progn
          (setf *error-msg* (format nil "Unexpected symbol '~a' at position ~d, expected '~a'" 
                                    (or current "EOF") *pos* expected))
          nil)))) 

;;; Parser Functions for Non-Terminals
(defun parse-G ()
  "Parse G → x | y | z | w."
  (let ((current (peek)))
    (cond
      ((equal current #\x) (match #\x))
      ((equal current #\y) (match #\y))
      ((equal current #\z) (match #\z))
      ((equal current #\w) (match #\w))
      (t (setf *error-msg* (or *error-msg* (format nil "Expected 'x', 'y', 'z', or 'w' at position ~d" *pos*)))
         nil))))

(defun parse-E-prime ()
  "Parse E' → oG E' | ε."
  (let ((start-pos *pos*))
    (if (match #\o)
        (if (and (parse-G) (parse-E-prime))
            t
            (setf *pos* start-pos))  ; in case if parsing fails
        t)))  ; ε production

(defun parse-E ()
  "Parse E → G E'."
  (format t "Parsing E at position ~d~%" *pos*)
  (if (parse-G)
      (parse-E-prime)
      (progn
        (format t "Error: Expected G at position ~d~%" *pos*)
        nil)))

(defun parse-L-prime ()
  "Parse L' → s L' | ε."
  (cond
    ((and (match #\s) (parse-L-prime)) t)
    (t t)))  ; ε production

(defun parse-L ()
  "Parse L → s L'."
  (if (match #\s)
      (parse-L-prime)
      nil))

(defun parse-S ()
  "Parse S → s | dLb."
  (let ((start-pos *pos*))
    (cond
      ((match #\s) t)
      ((and (match #\d) (parse-L) (match #\b)) t)
      (t (setf *pos* start-pos)
         (setf *error-msg* (or *error-msg* (format nil "Expected 's' or 'd' at position ~d" *pos*)))
         nil))))

(defun parse-S-prime ()
  "Parse S' → S | SeS'."
  (let ((start-pos *pos*))
    (cond
      ((parse-S) t)
      ((and (parse-S) (match #\e) (parse-S-prime)) t)  ; Continue parsing S'
      (t (setf *pos* start-pos)
         (setf *error-msg* (or *error-msg* (format nil "Expected 's', 'd', or 'e' at position ~d" *pos*)))
         nil))))

(defun parse-I ()
  "Parse I → iE S'. Must consume entire string."
  (if (and (match #\i)
           (parse-E)
           (parse-S-prime)
           (= *pos* (length *input*)))
      t
      (progn
        (setf *error-msg* (or *error-msg* (format nil "Incomplete parse at position ~d" *pos*)))
        nil)))

;;; Main Parsing Function
(defun parse (input-str)
  "Parse the input string and return T if valid, NIL with error message if invalid."
  (setf *input* (coerce input-str 'list))
  (setf *pos* 0)
  (setf *error-msg* nil)
  (if (parse-I)
      t
      (progn
        (format t "Error for '~a': ~a~%" input-str *error-msg*)
        nil)))

;;; Test Function
(defun test-parse (str)
  "Test a single string and print result."
  (format t "Parsing '~a': ~a~%" str (parse str)))

;;; Q7: Run Tests with Valid and Invalid Strings
(defun run-tests ()
  "Run tests for Q7: 7 valid and 7 invalid strings."
  (format t "=== Testing Valid Strings ===~%")
  (dolist (str '("ixoys" "ixdssb" "izowsss" "iwdssbes" "iyoydssbes" "ixoyowdsssbes" "ixoysdsssdsssdssb"))
    (test-parse str))
  
  (format t "~%=== Testing Invalid Strings ===~%")
  (dolist (str '("ixoy" "ixdsb" "izowssb" "iwdssbex" "iyoydssbex" "ixoyowdssbexs" "ixoysdsssdsssdsss"))
    (test-parse str)))

;;; Run the tests when the file is loaded
(run-tests)