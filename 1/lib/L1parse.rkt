#lang racket
(require srfi/13
         "L1types.rkt")

(define/contract (parse infile)
  (string? . -> . l1prog?)
  (let ([code (call-with-input-file infile read)])
    (parseL1prog code)))

(define/contract (parseL1prog prog)
  (list? . -> . l1prog?)
  (let ([main (parseMain (first prog))]
        [funs (map (lambda (x) (parseFun x #f)) (rest prog))])
    (l1prog (cons main funs))))

;; parseMain any → l1fun?
;; Hand off to parseFun, telling it to give this function the special name "go".
(define/contract (parseMain body)
  (list? . -> . l1fun?)
  (parseFun body #t))

;; parseFun any boolean? → l1fun?
;; The second arg indicates whether this function is the main one, and therefore
;; has no label/should be given the label "go".
(define/contract (parseFun body ismain)
  (-> list? boolean? l1fun?)
  (let ([name (if ismain `:go (first body))]
        [instrs (if ismain body (rest body))])
    (l1fun (label name)
           (map parseInstr instrs))))

;; parseInstr any → one of the instr types
(define/contract (parseInstr instr)
  (any/c . -> . l1instr?)
  (match instr
    [`(,x ,y ,z ,a ,b ,c)
     (cjump y z a b c)]
    [`(,x ,y ,z ,a ,b)
     (cmp x a z b)]
    [`(,x ,y ,z)
     (if (equal? y `<-)
         (if (list? z)
             (match (car z)
               [`print (print (cadr z))]
               [`allocate (allocate (cadr z) (caddr z))]
               [`array-error (array-error (cadr z) (caddr z))]
               [`mem (assign x (mem (cadr z) (caddr z)))])
             (assign (if (list? x)
                         (mem (cadr x) (caddr x))
                         x)
                     z))
         (mathop y x z))]
    [`(,x ,y)
     (match x
       [`goto (goto y)]
       [`call (call y)]
       [`tail-call (tail-call y)])]
    [`,x
     (if (list? x)
         (return)
         (label x))]))

(provide parse)
