
;; ##     ##    ###    ##       ##     ## ########  ######
;; ##     ##   ## ##   ##       ##     ## ##       ##    ##
;; ##     ##  ##   ##  ##       ##     ## ##       ##
;; ##     ## ##     ## ##       ##     ## ######    ######
;;  ##   ##  ######### ##       ##     ## ##             ##
;;   ## ##   ##     ## ##       ##     ## ##       ##    ##
;;    ###    ##     ## ########  #######  ########  ######

(define (make-tag name size)
  (assert 'make-tag
          (symbol? name)
          (non-negative-integer? size))
  (vector name size))

(define (tag-name tag)
  (assert 'tag-name (vector? tag))
  (vector-ref tag 0))

(define (tag-size tag)
  (assert 'tag-size (vector? tag))
  (vector-ref tag 1))

(define (v:cons kar kdr)
  (memory-push-roots! the-memory kar kdr)
  (let ((base (memory-allocate the-memory 3)))
    (memory-set! the-memory base (make-tag 'cons 3))
    (memory-set! the-memory (+ 1 base) kar)
    (memory-set! the-memory (+ 2 base) kdr)
    (memory-pop-roots! the-memory 2)
    (make-address base)))

(define (v:pair? v)
  (and (address? v)
       (let ((ptr (address-ptr v)))
         (eq? (tag-name (memory-ref the-memory ptr))
              'cons))))

(define (v:car kons)
  (assert 'v:car (v:pair? kons))
  (memory-ref the-memory (+ 1 (address-ptr kons))))

(define (v:cdr kons)
  (assert 'vcdr (v:pair? kons))
  (memory-ref the-memory (+ 2 (address-ptr kons))))

(define (v:list . vs)
  (if (null? vs)
      null
      (v:cons (car vs) (apply v:list (cdr vs)))))