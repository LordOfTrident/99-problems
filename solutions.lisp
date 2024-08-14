; Problem 01

(defun my-last (l)
	(if (cdr l)
		(last (cdr l))
		l))

(format t "-- 01 --~%")
(format t "~a~%" (my-last '(a b c d)))
(format t "~a~%" (my-last '()))


; Problem 02

(defun my-last-two (l)
	(when (cdr l)
		(if (cddr l)
			(my-last-two (cdr l))
			l)))

(format t "-- 02 --~%")
(format t "~a~%" (my-last-two '(a b c d)))
(format t "~a~%" (my-last-two '(a)))


; Problem 03

(defun my-at (l k)
	(cond
		((= k 1) (car l))
		(l       (my-at (cdr l) (1- k)))))

(format t "-- 03 --~%")
(format t "~a~%" (my-at '(a b c d e) 3))
(format t "~a~%" (my-at '(a) 3))


; Problem 04

(defun my-len (l)
	(defun loop_ (l acc)
		(if l
			(loop_ (cdr l) (1+ acc))
			acc))
	(loop_ l 0))

(format t "-- 04 --~%")
(format t "~d~%" (my-len '(a b c)))
(format t "~d~%" (my-len '()))


; Problem 05

(defun my-reverse (l)
	(defun loop_ (l acc)
		(if l
			(loop_ (cdr l) (cons (car l) acc))
			acc))
	(loop_ l ()))

(format t "-- 05 --~%")
(format t "~a~%" (my-reverse '(a b c)))


; Problem 06

(defun my-palindrome (l)
	(equal l (my-reverse l)))

(format t "-- 06 --~%")
(format t "~a~%" (my-palindrome '(x a m a x)))
(format t "~a~%" (my-palindrome '(a b)))


; Problem 07

(defun my-flatten (l)
	(defun loop_ (l acc)
		(cond
			((not l)        acc)
			((atom (car l)) (loop_ (cdr l) (cons  (car l) acc)))
			(t              (loop_ (cdr l) (loop_ (car l) acc)))))
	(my-reverse (loop_ l ())))

(format t "-- 07 --~%")
(format t "~a~%" (my-flatten '(a (b (c d) e))))


; Problem 08

(defun my-compress (l)
	(defun loop_ (l acc)
		(if (car l)
			(if (eq (car l) (cadr l))
				(loop_ (cdr l) acc)
				(loop_ (cdr l) (cons (car l) acc)))
			acc))
	(my-reverse (loop_ l ())))

(format t "-- 08 --~%")
(format t "~a~%" (my-compress '(a a a a b c c a a d e e e e)))
