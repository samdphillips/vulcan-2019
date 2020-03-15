
; (struct name (fields ...) )

(define (->string v)
  (cond
    ((string? v) v)
    ((symbol? v) (symbol->string v))
    ((number? v) (number->string v))
    (else
      (error "->string" v))))

(define (symbol-join . xs)
  (string->symbol
    (apply string-append (map ->string xs))))

(define (foldn n xs f)
  (cond
    ((null? xs) null)
    (else
      (cons (f n (car xs))
            (foldn (add1 n) (cdr xs) f)))))

(define (make-field-initializers field-names)
  (foldn 1 field-names
         (lambda (field-num field-name)
           `(memory-set! the-memory (+ ,field-num base) ,(car field-names)))))

(define (make-constructor struct-name field-names)
  (let* ((num-fields (length field-names))
         (size (add1 num-fields)))
    `(define (,struct-name ,@field-names)
      (memory-push-roots! the-memory ,@field-names)
      (let ((base (memory-allocate the-memory ,size)))
        (memory-set! the-memory base (make-tag ',struct-name ,size))
        ,@(make-field-initializers field-names)
        (memory-pop-roots! the-memory ,num-fields)
        (make-address base)))))

(define (make-predicate struct-name pred-name)
  `(define (,pred-name v)
    (and (address? v)
         (let ((ptr (address-ptr v)))
           (eq? (tag-name (memory-ref the-memory ptr))
                ',struct-name)))))

(define (make-field-accessor struct-name pred-name field-name field-num)
  (let ((accessor-name (symbol-join struct-name "-" field-name)))
    `(define (,accessor-name v)
      (assert ',accessor-name (,pred-name v))
      (memory-ref the-memory (+ ,field-num (address-ptr v))))))

(define (make-field-mutator struct-name pred-name field-name field-num)
  (let ((mutator-name (symbol-join "set-" struct-name "-" field-name "!")))
    `(define (,mutator-name v av)
      (assert ',mutator-name (,pred-name v))
      (memory-set! the-memory (+ ,field-num (address-ptr v)) av))))

(define (make-struct struct-name field-names)
  (let ((predicate-name (symbol-join struct-name "?")))
    `(,(make-constructor struct-name field-names)
      ,(make-predicate struct-name predicate-name)
      ,@(foldn 1 field-names
               (lambda (i n)
                 (make-field-accessor struct-name predicate-name n i)))
      ,@(foldn 1 field-names
               (lambda (i n)
                 (make-field-mutator struct-name predicate-name n i))))))
