#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm, v 1.13 2020-01-10 12:51:12-08 - - $
;;
;; NAME
;;      sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;      sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed. Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

;; Hash Table storing functions
(define *function-table* (make-hash))
;; Hash Table storing variables
(define *variable-table* (make-hash))
;; Hash Table storing arrays
(define *array-table* (make-hash))
;; Hash Table storing addresses of lines 
(define *label-table* (make-hash))

;; setters and getters for hash tables
(define (function-get key)
        (hash-ref *function-table* key))
(define (variable-get key)
        (hash-ref *variable-table* key))
(define (array-get key)
        (hash-ref *array-table* key))
(define (label-get key)
        (hash-ref *label-table* key))
;;initialize functions
(for-each
    (lambda (symfun) (hash-set! *function-table* 
                          (car symfun) (cadr symfun)))
    `(
        (+ ,+)(- ,-) (* ,*) (/ ,/)
        (<= ,<=) (>= ,>=) (= ,=) (> ,>) (< ,<) 
        (!= , (lambda (x y) (not (= x y))))
        (sin ,sin) (cos ,cos) (tan ,tan)
        (asin ,asin) (acos ,acos) (atan ,atan)
        (abs ,abs) (ceiling ,ceiling) (exp ,exp)
        (floor ,floor) (log ,log) (round ,round)
        (sqrt ,sqrt) (^ ,expt)
    ))

;; initialize variables
(for-each
    (lambda (varval)
        (hash-set! *variable-table* (car varval) (cadr varval)))
    `(
        (nan ,(/ 0.0 0.0)) 
        (eof ,0.0)
        (pi    ,(acos -1))
        (e     ,(exp 1))
        (i     ,(sqrt -1))
        (one   1)
        (zero  0)
    ))

(define NAN (/ 0.0 0.0))

(define (evaluate-expression expr);; taken from teachers examples
    (cond ((number? expr) (+ expr 0.0))
          ((symbol? expr) (hash-ref *variable-table* expr NAN))
          ((pair? expr) 
              (let ((func (hash-ref *function-table* (car expr) NAN))
                    (opnds (map evaluate-expression (cdr expr))))
                   (if (null? func) NAN
                       (apply func (map evaluate-expression opnds)))))
           (else NAN)))

;; Gets input file name
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

;; Exit, KILL message
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

;; Error message if there is no input file
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;; reads inputfile into list. 
;; One line on the file is one item on the list
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             ;; reads file into a list called program. 
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                   ;; return program
                         program))))


(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))

;; prints input file line for line
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

;; initialize labels into *label-table*
(define initialize-labels
      (lambda (program)
        (when (or (= 3 (length (car program))) 
           (= 2 (length (car program))))
                (hash-set! *label-table* (cadr (car program)) program))
        (when ( > (length (cdr program)) 0)
              (initialize-labels (cdr program)))))

;; interpret program line by line
(define interpret-program 
     (lambda (program)
     (let interpret-next ((program program))
        (when (not (null? program))
             ;skips lines without statement
             (when ( > (length (car program)) 1) 
                 (when (and ( = ( length (car program)) 2) 
                  (pair? (cadr (car program))))
                    (define keyword (car (cadr (car program))))
                    (interpret-keyword keyword (cadr (car program))))
                 (when ( = (length (car program)) 3)
                    (define keyword (car (caddr( car program))))
                    (interpret-keyword keyword (caddr (car program)))))
     ; recursive loop to next line in program
     ; The exit is so that the goto statement gives full control
     (if (null? (cdr program)) (exit 0)
             (interpret-program (cdr program)))))))

;; send to proper interpret-statement 
(define (interpret-keyword keyword statement)  
        (cond
                ((eqv? keyword 'dim)
                        (interpret-dim (cdr statement)))
                ((eqv? keyword 'let)
                        (interpret-let (cdr statement)))
                ((eqv? keyword 'goto)
                        (interpret-goto (cdr statement)))
                ((eqv? keyword 'if)
                        (interpret-if (cdr statement)))
                ((eqv? keyword 'print)
                        (interpret-print (cdr statement)))
                ((eqv? keyword 'input)
                        (interpret-input (cdr statement)))))

;; read in input, taken from teachers examples
(define (readnumber)
     (let ((object (read)))
          (cond [(eof-object? object) object 
            (hash-set! *variable-table* eof 1.0)]
               [(number? object) (+ object 0.0)]
               [else (begin (printf "invalid number: ~a~n" object)
                       (readnumber))])))

;; take in input and store it into variable
(define (interpret-input statement)
        (let ((input (readnumber)))
                (hash-set! *variable-table* (car statement) input))
        (when (> (length statement) 1)
                (interpret-input (cdr statement))))

;; interpret statement with print
;; print out expressions, strings, numbers
(define (interpret-print statement)
        (let loop ((statement statement))
           (when (pair? statement)
                (if (hash-has-key? *variable-table* (car statement))
                    (display (variable-get (car statement)))
                    (if (list? (car statement))
                        (display (evaluate-expression (car statement)))
                        (display (car statement))))
               (loop (cdr statement))))
           (newline))

;interpret statement with let
;assign value to variable
(define (interpret-let statement)
           (hash-set! *variable-table* (car statement)
               (evaluate-expression (cadr statement))))

; interpret statement with dim, creates an array and puts it into 
; array table
(define (interpret-dim statement)
    (hash-set! *array-table* (cadr(car statement))
         ;third element
         (make-vector (exact-round (caddr(car statement))) 0.0 ))
            ; (display (vector-ref (hash-ref *array-table* (cadr(car statement))) 1))
)

;interpret statement with goto
; jumps to program line if it exists
(define (interpret-goto statement)
        (if (hash-has-key? *label-table* (car statement))
                (interpret-program (label-get (car statement)))
                (die '("Error, no jump"))))

; interpret statement with if
; if condition is true, jump to line
(define (interpret-if statement)
        (when (eqv? (evaluate-expression (car statement)) #t)
                  (interpret-program (label-get (cadr statement)))))

;; main method
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
              (program (readlist-from-inputfile sbprogfile)))
              (initialize-labels program)
              (interpret-program program))))
             ; (write-program-by-line sbprogfile program))))

;; call main method
(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))

