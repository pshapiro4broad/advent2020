;;; -*- lexical-binding: t -*-

(setq input
      (with-temp-buffer
        (insert-file-contents "day2-input.txt")
        (split-string (buffer-string) "\n" t)))

(defun chars-in-string (char string)
  (cl-loop
   for c across string
   count (= c char)))

;; (car input)
;; => "2-6 c: fcpwjqhcgtffzlbj"

(defun parse-entry (entry)
  (let* ((split (split-string entry ": "))
         (password (cadr split))
         (rule (split-string (car split) " "))
         (range (split-string (car rule) "-"))
         (first (string-to-number (car range)))
         (second (string-to-number (cadr range)))
         (char (elt (cadr rule) 0)))
  (list password char first second)))

(defun count-all (predicate)
  (cl-loop
   for entry in input
   count (apply predicate (parse-entry entry))))

(defun is-valid1 (password char min max)
  (let ((num-chars (chars-in-string char password)))
    (and (>= num-chars min) (<= num-chars max))))

(defun part1 ()
  (count-all 'is-valid1))

(part1)
;; ==> 582

(defun is-valid2 (password char first second)
  (not (eq (= char (elt password (1- first)))
           (= char (elt password (1- second))))))

(defun part2 ()
  (count-all 'is-valid2))

(part2)
;; ==> 729
