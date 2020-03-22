#!/usr/bin/racket
;;#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
#lang racket
;; $Id: sbi.scm,v 1.20 2020-01-16 13:38:52-08 - - $
;;
;; AUTHOR
;;    Sasank Madineni (smadinen)
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

;;---------------------------------------------------------------------
;;--Standard Input/Output----------------------------------------------
;;---------------------------------------------------------------------
(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))
(define *arg-list* (vector->list (current-command-line-arguments)))

;;---------------------------------------------------------------------
;;--Hash Tables--------------------------------------------------------
;;---------------------------------------------------------------------
(define *function-table* (make-hash))
(define *variable-table* (make-hash))
(define *array-table* (make-hash))
(define *label-table* (make-hash))

;;initializing *function-table*
(for-each
  (lambda (element)
    (hash-set! *function-table* (car element) (cadr element)))
    `(
      ;;binary operators
      (+, +)
      (-, -)
      (*, *)
      (/, /)
      (%, (lambda (a b) (- a (* (/ a b) b))))
      (^, expt)
      (=, equal?)
      (>, >)
      (<, <)
      (>=, >=)
      (<=, <=)
      (!=, (lambda (a b) (not (equal? a b))))
      ;;builtin symbols
      (abs, abs)
      (acos, acos)
      (asin, asin)
      (atan, atan)
      (ceiling, ceiling)
      (cos, cos)
      (exp, exp)
      (floor, floor)
      (log, (lambda (a) (log (+ a 0.0))))
      (round, round)
      (sin, sin)
      (sqrt, sqrt)
      (tan, tan)
      (truncate, truncate)
      ))

;;variable-table declaration
(for-each
    (lambda (element) (hash-set! *variable-table*
        (car element) (cadr element)))
    `(
      (nan, (/ 0.0 0.0))
      (eof, 0.0)
      (pi, (acos -1))
      (e, (exp 1))
    ))

;;---------------------------------------------------------------------
;;--Defined Functions--------------------------------------------------
;;---------------------------------------------------------------------

;;prints newline
(define (printn) (printf "~n"))

;;returns the first element of the line (line number)
(define (get-linenumber program) (caar program))

;;returns the first element of the program (line)
(define (get-line program) (car program))

;;returns the next line(s) in the program
(define (get-next line) (cdr line))

;;reference function -DO NOT USE-
;;returns the length of the line that was passed in
(define (get-length line) (length line))

(define NAN (/ 0.0 0.0))

;; function: interpret-program
;; description: loops through <program> via tail recursion
;;              and calls interpret-statement on every statement found
;; parameters: program - the program contents list to loop through
(define (interpret-program program)
    (define (interpret-program. program.)
        (when (not (null? program.))
            (let* ((line (get-line program.)))
                ;;making sure line is not empty before parsing.
                (when (not (null? (cdr line)))
                    ;;checking to see if the line should be parsed
                    (cond
                    [(pair? (cadr line))
                        (interpret-statement (cadr line))
                    ]
                    [(> (length line) 2)
                      (cond
                      [(pair? (caddr line))
                        (interpret-statement (caddr line))
                      ]
                      )
                    ]
                    )
                )
                (if (null? (cdr program.))
                    (exit 0) ;;CASE EOF
                (interpret-program. (cdr program.))
                )
            )
        )
    )
    (interpret-program. program)
)

;; function: interpret-statement
;; description: takes a statement and calls the appropriate
;;              interpret-<statement> function
;; parameters: statement - the statement that needs to be interpreted
(define (interpret-statement statement)
    ; (printf "Interpreting Statement: ~s~n" statement)
    (cond
    [(eqv? (car statement) 'print)
        (if (null? (cdr statement))
            (printf "Error: No Print Statement Provided~n")
        ;;else
            (interpret-print (get-next statement))
        )
    ]
    [(eqv? (car statement) 'let)
        (interpret-let (cadr statement) (caddr statement))
    ]
    [(eqv? (car statement) 'dim)
        (interpret-dim (cadadr statement) (cadr (cdadr statement)))
    ]
    [(eqv? (car statement) 'goto)
        (interpret-goto (cadr statement))
    ]
    [(eqv? (car statement) 'if)
        (interpret-if (cadr statement) (caddr statement))
    ]
    [(eqv? (car statement) 'input)
        (interpret-input (cdr statement))
    ]
    )
)

;;function: evaluate-expression
;;description: takes in an <expression> and evaluates it by
;;             checking the function, variable and array tables
;;parameters: expression - the expression that is going to be
;;                         evaluated
(define (evaluate-expression expression)
    ; (printf "Evaluating Expression ~s~n" expression)
    (cond
    [(number? expression)
        (+ expression 0.0)
    ]
    [(symbol? expression)
        (hash-ref *variable-table* expression NAN)
    ]
    [(pair? expression)
        (cond
        [(hash-has-key? *function-table* (car expression))
            (let ((func (hash-ref *function-table*
              (car expression) NAN))
                  (opnds (map evaluate-expression (cdr expression))))
                  (if (null? func) NAN
                  ;;else
                  (apply func (map evaluate-expression opnds))
              )
            )
        ]
        ;else if: check array-table
        [(hash-has-key? *array-table* (cadr expression))
            (let ( ;;let definitions
                (vec (hash-ref *array-table* (cadr expression) NAN))
                (pos (inexact->exact
                    (evaluate-expression (caddr expression))))
                )

                (vector-ref vec pos)
            )
        ]
        [ else
          (die '(Invalid Expression))
        ]
        )
    ]
    )
)

;;function: test-expression
;;description: runs evaluate-expression on <expression>
;;parameters: exprsesion - the expression being evaluated
(define (test-expression expression)
    (printf "expression: ~s~n" expression)
    (printf "value: ~s~n" (evaluate-expression expression))
    (newline)
)

;;function: interpret-goto
;;description: checks the *label-table* for <label> and jumps to it
;;parameters: label - the label being searched for in the label-table
(define (interpret-goto label)
    (if (hash-has-key? *label-table* label)
      (interpret-program (hash-ref *label-table* label))
      ;;else
      (die '("Error: Label Not Found"))
    )
)

;;function: interpret-if
;;description: if <arguments> results in a true expression
;;             then jumps to <label>
;;parameters: arguments - the arguments for the if statement
;;            label - the label to jump to
(define (interpret-if arguments label)
    (when ((hash-ref *function-table* (car arguments))
        (evaluate-expression (cadr arguments))
        (evaluate-expression (caddr arguments)))
        (interpret-goto label)
    )
)

;; function: interpret-print
;; description: prints out the statement passed to the console
;; parameters: statement - the statement that needs to be printed
(define (interpret-print statement)
    (define (interpret-print. statement.)
      (if (null? statement)
          (printn)
      ;;else
          (cond [(string? (car statement))
              (display (car statement))
          ]
          [else
              (display (evaluate-expression (car statement)))
          ]
          )
      )
      (if (null? (cdr statement.))
          (printn)
          ;;else
          (interpret-print (cdr statement.))
      )
    )
    (interpret-print. statement)
)

;;function: interpret-let
;;description: assigns <value> to <variable> in variable/array table
;;parameters: variable - the key for the table
;;            value - the value for the table
(define (interpret-let variable value)
    (if (list? variable)
        (vector-set! (hash-ref *array-table* (cadr variable) NAN)
            (inexact->exact (evaluate-expression
            (caddr variable))) value)
    ;;else
        (hash-set! *variable-table* variable
            (evaluate-expression value))
    )
)

;;function: interpret-dim
;;description: creates an array <variable> of size <value>
;;             in *array-table*
;;parameters: variable - the key for the *array-table*
;;            value - the size of the array
(define (interpret-dim variable value)
    ;;create a vector in  *array-table*
    ;;with name <variable> and size <value>
    (hash-set! *array-table* variable (make-vector value))
)

;;function: interpret-input
;;description: waits for user input and returns the value inputted
;;parameters: variables - the vars to store the value that is inputted
(define (interpret-input variables)
    (define (interpret-input. variables. num_vars)
        (printf "TODO")
    )
    (interpret-input. variables)
)

;;function: make-label-table
;;description: generates the label-table by recursively
;;             searching through <program>
;;parameters: program - the program to search through
(define (make-label-table program)
    (define (make-label-table. program.)
        (when (not (null? program.))
            ;;ignoring blank lines
            (cond [(> (length (car program.)) 1)
                (cond [(not(list? (cadar program.)))
                    (hash-set! *label-table* (cadar program.) program.)
                ]
                )
            ]
            )
            (make-label-table. (cdr program.))
        )
    )
    (make-label-table. program)
)

;;---------------------------------------------------------------------
;;--Predefined Functions-----------------------------------------------
;;---------------------------------------------------------------------

;;function : *run-file*
;;description :
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

;;function : die
;;description : displays the contents in <list> as a *stderr* message
;;parameters : list - a list that is iterated over
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

;;function : usage-exit
;;description : calls die on the current file that is running
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;;function : readlist-from-inputfile
;;description : reads from <filename> and outputs to a list
;;parameters : filename - the input filename
(define (readlist-from-inputfile filename)
    ;;set inputfile = open(filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile)) ;;if can't open inputfile
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))
;;function : dump-stdin
;;description : prints out current value stored in *stdin*
(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))

;;function : write-program-by-line
;;description : output the sb file
;;parameters : filename - the name of the file that is being parsed
;;             program - the contents of the file in list format
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~a~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~a~n" line)) program)
    (printf ")~n"))

;;function : main
;;description : main function
;;parameters : arglist - the cli arguments that are passed in
(define (main arglist)
     ;;arglist == NULL || (arglist[1] != NULL)
    ; (test-expression (exp 1))
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist)) ;;sbprogfile = arglist[0]
               (program (readlist-from-inputfile sbprogfile)))
               ;;(write-program-by-line sbprogfile program)
               (make-label-table program)
               ;(display *label-table*)
               ;(printn)
               (interpret-program program)
        )
     )
)

(main *arg-list*)
