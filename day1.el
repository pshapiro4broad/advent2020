;;; -*- lexical-binding: t -*-

(setq input
      (with-temp-buffer
        (insert-file-contents "day1-input.txt")
        (mapcar 'string-to-number (split-string (buffer-string) "\n" t))))

(defun part1 ()
  (apply '* (let ((m 0))
              (cl-loop
               for n in input do
               (setq m (cl-loop
                        for m in input
                        until (= (+ n m) 2020)
                        finally return m))
               until (= (+ n m) 2020)
               finally return (list n m)))))
(part1)
;; ==> 1014171

(defun part2 ()
  (cl-loop for n in input do
           (cl-loop for m in input do
                    (cl-loop for o in input
                             until (= (+ n m o) 2020)
                        finally (if (= (+ n m o) 2020) (print (list n m o)))))))

(part2)

(59 1395 566)

(59 566 1395)

(1395 59 566)

(1395 566 59)

(566 59 1395)

(566 1395 59)
nil

(apply '* '(566 1395 59))
;; ==> 46584630
