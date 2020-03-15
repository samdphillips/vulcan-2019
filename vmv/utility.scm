;; ##     ## ######## #### ##       #### ######## #### ########  ######
;; ##     ##    ##     ##  ##        ##     ##     ##  ##       ##    ##
;; ##     ##    ##     ##  ##        ##     ##     ##  ##       ##
;; ##     ##    ##     ##  ##        ##     ##     ##  ######    ######
;; ##     ##    ##     ##  ##        ##     ##     ##  ##             ##
;; ##     ##    ##     ##  ##        ##     ##     ##  ##       ##    ##
;;  #######     ##    #### ######## ####    ##    #### ########  ######

(define assertions-enabled? #t)

(macro assert
  (lambda (form)
    (let ((tag (cadr form))
          (the-asserts (cddr form)))
      `(begin
        ,@(map (lambda (e) `(assert/1 ,tag ,e)) the-asserts)))))

(macro assert/1
  (lambda (form)
    (let ((test (caddr form))
          (tag  (cadr form)))
      `(when assertions-enabled?
         (unless ,test
           (error "assertion failed" ,tag ',test))))))

(define (displayln v)
  (display v) (newline))

(define null '())

(define (sub1 x) (- x 1))
(define (add1 x) (+ x 1))

(define (non-negative-integer? v)
  (and (integer? v) (not (negative? v))))
