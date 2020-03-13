
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

;;    ###    ########  ########  ########  ########  ######   ######
;;   ## ##   ##     ## ##     ## ##     ## ##       ##    ## ##    ##
;;  ##   ##  ##     ## ##     ## ##     ## ##       ##       ##
;; ##     ## ##     ## ##     ## ########  ######    ######   ######
;; ######### ##     ## ##     ## ##   ##   ##             ##       ##
;; ##     ## ##     ## ##     ## ##    ##  ##       ##    ## ##    ##
;; ##     ## ########  ########  ##     ## ########  ######   ######

(define (make-address ptr) (vector 'address ptr))

(define (address? a)
  (and (vector? a)
       (= 2 (vector-length a))
       (eq? 'address (vector-ref a 0))))

(define (address-ptr addr)
  (assert 'address-ptr (address? addr))
  (vector-ref addr 1))

(define (set-address-ptr! addr ptr)
  (assert 'set-address-ptr! (address? addr))
  (vector-set! addr 1 ptr))

;; ##     ## ######## ##     ##  #######  ########  ##    ##
;; ###   ### ##       ###   ### ##     ## ##     ##  ##  ##
;; #### #### ##       #### #### ##     ## ##     ##   ####
;; ## ### ## ######   ## ### ## ##     ## ########     ##
;; ##     ## ##       ##     ## ##     ## ##   ##      ##
;; ##     ## ##       ##     ## ##     ## ##    ##     ##
;; ##     ## ######## ##     ##  #######  ##     ##    ##

(define (make-memory size)
  (vector 0 null (make-vector size #f)))

(define (memory-free-ptr memory)
  (assert 'memory-free-ptr (vector? memory))
  (vector-ref memory 0))

(define (set-memory-free-ptr! memory v)
  (assert 'set-memory-free-ptr!
          (vector? memory)
          (non-negative-integer? v))
  (vector-set! memory 0 v))

(define (memory-roots memory)
  (assert 'memory-roots (vector? memory))
  (vector-ref memory 1))

(define (set-memory-roots! memory roots)
  (assert 'set-memory-roots!
          (vector? memory)
          (list? roots))
  (vector-set! memory 1 roots))

(define (memory-push-roots! memory . vs)
  (assert 'memory-push-roots! (vector? memory))
  (set-memory-roots! memory (append vs (memory-roots memory))))

(define (memory-pop-roots! memory n)
  (assert 'memory-pop-roots!
          (vector? memory)
          (non-negative-integer? n))
  (let loop ((n n) (roots (memory-roots memory)))
    (cond
      ((zero? n) (set-memory-roots! memory roots))
      (else
        (loop (- n 1) (cdr roots))))))

(define (memory-cells memory)
  (assert 'memory-cells
          (vector? memory))
  (vector-ref memory 2))

(define (memory-size memory)
  (assert 'memory-size (vector? memory))
  (vector-length
    (memory-cells memory)))

(define (memory-ref memory ptr)
  (assert 'memory-ref
          (vector? memory)
          (non-negative-integer? ptr))
  (vector-ref (memory-cells memory) ptr))

(define (memory-set! memory ptr val)
  (assert 'memory-set!
          (vector? memory)
          (non-negative-integer? ptr))
  (vector-set! (memory-cells memory) ptr val))

(define (copy-memory-cells! old-memory new-memory)
  (let ((free-ptr (memory-free-ptr old-memory)))
    (let copy! ((i 0))
      (unless (= i free-ptr)
        (memory-set! new-memory i (memory-ref old-memory i))
        (copy! (add1 i))))))

(define (memory-allocate memory size)
  (assert 'memory-allocate
          (vector? memory)
          (non-negative-integer? size))
  (memory-allocate/fail memory size collect-garbage!))

(define (memory-allocate/fail memory size failk)
  (assert 'memory-allocate/fail
          (vector? memory)
          (non-negative-integer? size))
  (let* ((ptr (memory-free-ptr memory))
         (free-ptr (+ size ptr)))
    (cond
     ((>= free-ptr (memory-size memory)) (failk memory size))
     (else
       (set-memory-free-ptr! memory free-ptr)
       ptr))))

;;  ######      ###    ########  ########     ###     ######   ########
;; ##    ##    ## ##   ##     ## ##     ##   ## ##   ##    ##  ##
;; ##         ##   ##  ##     ## ##     ##  ##   ##  ##        ##
;; ##   #### ##     ## ########  ########  ##     ## ##   #### ######
;; ##    ##  ######### ##   ##   ##     ## ######### ##    ##  ##
;; ##    ##  ##     ## ##    ##  ##     ## ##     ## ##    ##  ##
;;  ######   ##     ## ##     ## ########  ##     ##  ######   ########
;;
;;  ######   #######  ##       ##       ########  ######  ######## ####  #######  ##    ##
;; ##    ## ##     ## ##       ##       ##       ##    ##    ##     ##  ##     ## ###   ##
;; ##       ##     ## ##       ##       ##       ##          ##     ##  ##     ## ####  ##
;; ##       ##     ## ##       ##       ######   ##          ##     ##  ##     ## ## ## ##
;; ##       ##     ## ##       ##       ##       ##          ##     ##  ##     ## ##  ####
;; ##    ## ##     ## ##       ##       ##       ##    ##    ##     ##  ##     ## ##   ###
;;  ######   #######  ######## ######## ########  ######     ##    ####  #######  ##    ##

(define (gc-error memory size)
  (error "couldn't allocate enough space during gc" (memory-size memory) size))

(define (collect-garbage! memory size)
  (let ((new-memory (make-memory (memory-size memory)))
        (roots (memory-roots memory)))
    (copy-roots! roots memory new-memory)
    (trace-memory! memory new-memory)
    (set! the-memory new-memory)
    (memory-allocate/fail new-memory size enlarge-memory!)))

(define (copy-roots! roots old-memory new-memory)
  (for-each
    (lambda (addr)
      (when (address? addr)
        (let ((new-addr (copy-object! addr old-memory new-memory)))
          (set-address-ptr! addr (address-ptr new-addr)))))
    roots)
  (set-memory-roots! new-memory roots))

(define (trace-memory! old-memory new-memory)
  (let trace-objects! ((scan-ptr 0))
    (unless (= (memory-free-ptr new-memory) scan-ptr)
      (let ((next-ptr (trace-object! scan-ptr old-memory new-memory)))
        (trace-objects! next-ptr)))))

(define (trace-object! ptr old-memory new-memory)
  (let ((size (tag-size (memory-ref new-memory ptr))))
    (let trace-slots! ((size (sub1 size)) (ptr (add1 ptr)))
      (cond
        ((zero? size) ptr)
        (else
          (let* ((old-slot-value (memory-ref new-memory ptr))
                 (new-slot-value (copy-object! old-slot-value
                                               old-memory new-memory)))
            (memory-set! new-memory ptr new-slot-value)
            (trace-slots! (sub1 size) (add1 ptr))))))))

(define (copy-object! addr old-memory new-memory)
  (if (not (address? addr))
      addr
      (let* ((old-ptr (address-ptr addr))
             (tag     (memory-ref old-memory old-ptr)))
        (cond
          ((address? tag) tag)
          (else
            (let* ((size     (tag-size tag))
                   (new-ptr  (memory-allocate/fail new-memory size gc-error))
                   (new-addr (make-address new-ptr)))
              (copy-words! size old-memory old-ptr new-memory new-ptr)
              (memory-set! old-memory old-ptr new-addr)
              new-addr))))))

(define (copy-words! size old-memory old-ptr new-memory new-ptr)
  (let do-copy! ((size size) (old-ptr old-ptr) (new-ptr new-ptr))
    (unless (zero? size)
      (memory-set! new-memory new-ptr (memory-ref old-memory old-ptr))
      (do-copy! (sub1 size) (add1 old-ptr) (add1 new-ptr)))))

(define (enlarge-memory! memory size)
  (let* ((new-size (quotient (* (+ (memory-size memory) size) 3) 2))
         (new-memory (make-memory new-size)))
    (set-memory-free-ptr! new-memory (memory-free-ptr memory))
    (set-memory-roots! new-memory (memory-roots memory))
    (copy-memory-cells! memory new-memory)
    (set! the-memory new-memory)
    (memory-allocate/fail new-memory size gc-error)))

(define the-memory #f)

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

(set! the-memory (make-memory 1024))
