#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;22!
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
(define *stderr* (current-error-port))
;;(define iterator 0)
;;(define current 0)
(define *iterator-table* (make-hash))
(define (iterator-get key)
        (hash-ref *iterator-table* key #f))

(define *line-table* (make-hash))
(define (line-get key)
        (hash-ref *line-table* key))

(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key #f))


(define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key #f))

(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key #f))

(define (add1)
        (hash-set! *iterator-table* 'current (+ (iterator-get 
          'current) 1))
)

(define (change line)
        (hash-set! *iterator-table* 'current line)
)

(define (iterate)
        (hash-set! *iterator-table* 'iterator (+ (iterator-get 
          'iterator) 1))
)

(define (addLength)
        (hash-set! *iterator-table* 'cmdLen (+ (iterator-get 'cmdLen)
         1))
)

(define (resetLength)
    (hash-set! *iterator-table* 'cmdLen 0)
)

(define (addptr)
        (hash-set! *iterator-table* 'vpointer (+ (iterator-get 
          'vpointer) 1))
)

(define (resetptr)
    (hash-set! *iterator-table* 'vpointer 0)
)

(define (ft-set key value)
  (hash-set! *function-table* key value)
)

(define (vt-set key value)
  (hash-set! *variable-table* key value)
)

(define cmdVec (make-vector 50))
;;;;;;;;;;;;;;;;;;;;
;END OF DEFINITIONS
;;;;;;;;;;;;;;;;;;;;

(vt-set 'inputcount 0)
(vt-set 'e 2.718281828459045235360287471352662497757247093)
(vt-set 'pi 3.141592653589793238462643383279502884197169399)


(ft-set '+ +)
(ft-set 'div (lambda (x y) (floor (/ x y))))
(ft-set 'log10 (lambda (x) (/ (log x) (log 10.0))))
(ft-set '% (lambda (x y) (- x (*(truncate (/ x y)) y))))
(ft-set '- -)
(ft-set '+ +)
(ft-set '/ (lambda(x y) (/ x (+ y 0.0))))
(ft-set '* *)
(ft-set '^ expt)
(ft-set 'ceil ceiling)
(ft-set 'exp exp)
(ft-set 'floor floor)
(ft-set 'log  (lambda (x) (if (= x 0) -inf.0 (log x))))
(ft-set 'log2 (lambda (x) (/ (log x) (log 2.0))))
(ft-set 'tan tan)
(ft-set 'abs abs)
(ft-set 'acos acos)
(ft-set 'asin asin)
(ft-set 'atan atan)
(ft-set 'cos cos)
(ft-set 'round round)
(ft-set 'sin sin)
(ft-set 'trunc truncate)        
(ft-set 'sqrt sqrt) 
(ft-set '= =)
(ft-set '< <) 
(ft-set '> >) 
(ft-set '<= <=)
(ft-set '>= >=)
(ft-set '<> (lambda (x y) (not(= x y))))   


;;;;;;;;;;;;;;;;;;;;
;END OF FUNCTION TABLE
;;;;;;;;;;;;;;;;;;;;


(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;;;;;;;;;;;;;;;;;;;;
;End of Given from mack
;HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;
(define (what-kind value)
    (cond ((real? value) 'real)
          ((vector? value) 'vector)
          ((procedure? value) 'procedure)
          (else 'other)))


(define (atom? x) (not (or (pair? x) (null? x))))

(define (print-vt)
  (hash-for-each *variable-table*
        (lambda (key value)
                (printf "~s : ~s = ~s~n" key (what-kind value) value))
    )
)
;;;;;;;;;;;;;;;;;;;
;;DEFINE COMMANDS;;
;;;;;;;;;;;;;;;;;;;

(define (do-expr expr)
 ; (printf "do-expr: ~s~n" expr)
   (cond ((string? expr) expr)
         ((number? expr) expr)
         ((symbol? expr) (variable-get expr))
         ((pair? expr)
          (if(equal? (function-get (car expr)) #f)
              (vector-ref (variable-get (car expr)) (- (do-expr (cadr 
                expr)) 1))
            (apply (function-get (car expr))
                                (map do-expr (cdr expr)))))
         (else #f)
    )
)

(define (do-let expr)
  ;(printf "do-let:~s~n" expr)
  (if(atom? (car expr))
    (begin ;this is not an array
      (hash-set! *variable-table* (car expr) (do-expr(car(cdr expr))))
    )
      (vector-set! (variable-get (car(car expr))) (-(do-expr (cadr (
      car expr)))1) (do-expr(car(cdr expr))));this is for array
  )                 ;name of array        location        value
; (print-vt)
)


(define(goto label)
  (begin
    ;(printf "GOTO:~s~n" label)
    (define jump (label-get label)) ;; get line to jump to
    (define actualJump(- jump 1))   ;; because current is iterated at
    ; the beginning of execute-command
    (change actualJump)   ;; set 'current to correct value
    (execute)  ;; begin executing again
  )
)

(define (do-print args)
 ; (print-vt)
  (if (equal? args '())
    '()
    (begin
      (printf "~a" (do-expr(car args)))
      (if (equal? (cdr args) '())
        '()
        (do-print (cdr args))
      )
    )
  )
)
(define (do-dim args)
  ;(printf "do-dim:~s~n" args)
  (define v (make-vector (do-expr (cadr args))))
  (hash-set! *variable-table* (car args) v) 
)  ;vector of correct size

(define(do-if args)
  ;(printf "do-if:~s~n" args)
  ;(printf "input count : ~s~n" (variable-get 'inputcount))
  (define cmd1 (car args))
  ;(printf "~s~n" (do-expr cmd1))
  (if (do-expr cmd1)
    (goto (car(cdr args)))
    '()
  )
)

(define (do-input-op expr count)
  ;(printf "expr ~s" expr)
  (if (null? expr)
    (begin
      (if (equal? expr '())
        (execute)
        count
      )
      count
    )
     (let ((input (read)))
        (if (or (eof-object? input) (not(number? input)))
          (begin
            (printf "Expected number given: ~s~n" input)
            (vt-set 'inputcount -1)
          )
          (begin
            (vt-set (car expr) input)
            (vt-set 'inputcount (+ 1 (variable-get 'inputcount)))
            (do-input-op (cdr expr) count)
          )
        )
      )
   )
)

(define (do-input expr) ; Take input.
    (vt-set 'inputcount 0)
  (if (null? (car expr))
    (vt-set 'inputcount -1)
    (do-input-op expr 0)
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;
;; END DEFINE COMMANDS;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (execute-command cmnd args) ;Executes the global vec (cmdVec)
; which holds the current command
    (begin
      (case cmnd
        ((let) (do-let args))
        ((input) (begin(do-input args)))
        ((dim) (do-dim (car args)))
        ((goto) (goto (car args)))
        ((if) (begin (do-if args)))
        ((print)(begin (do-print args) (printf "~n")))
        (else (printf "Error: ~s is not a function" cmnd))
      )
    )
)


(define (parse-command command) ;parses the actual command and splits
; into atoms puts in vector
    (begin
      (if (atom? (car command))
        (begin 
        ;  (printf "cmd ~s~n" (car command))
          (execute-command (car command) (cdr command)) 
        )
          (parse-command (car command))
      )
    )
  ;)
)

(define (execute)
  (resetLength)  ;; this being down there was causing the error, 
  ;cmd vector was not being reset causing issues.....
  (add1)  ;+1 to current
  (define i (iterator-get 'current))  ;i = current
  (if (eqv? i (iterator-get 'iterator)) ;if (i = # of line)
    ;;(printf "blarghhhh~n")  ;then end
    (exit) ;; was not exiting with your print statement was jumping 
    ;immediately to bleep afterwards?? strange
    (begin  ;else if the line is null print hurr
      (if (equal? (line-get i) '())
        '()
        (begin
          (parse-command (line-get i))  ;parse command into global 
          ;vec also sets length of vector
          (resetptr)   ;reset pointer to go through the vector again
          
          
        )
      )
    ;;(printf "bleeeppp~n")
    (execute)
    )
  )
)

(define (parse-Hash command lineNr) ;Parses for the label
  ;(printf "line command : ~s~n" command)
  (if (equal? (cdr command) '())
    (begin
      (if (atom? (car command)) ;if its atomic it is a line with label
        (hash-set! *label-table* (car command) lineNr) ;add to LT
        (hash-set! *line-table* lineNr (car command)) ;else + 2 lineT
      )
    )
    (begin  ;if there is a label pull the label our put it in the hash
      (hash-set! *label-table* (car command) lineNr)
      ;Add cdr command to the function hash with line Nr
      (hash-set! *line-table* lineNr (cdr command))
    )
  )
)

(define (parse-line line) ;initial line parsing checks if 
  ;there is content in the line
  (iterate)
  (hash-set! *line-table* (car line) '())     
  (if(equal? (cdr line) '()) ;if a command exists, parse it
    '()
    (parse-Hash (cdr line) (car line))
  )
)

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (map (lambda(line)(parse-line line)) program))

(define (main arglist)
    (hash-set! *iterator-table* 'iterator 1)  ;initialize iterators
    (hash-set! *iterator-table* 'current 0)
    (hash-set! *iterator-table* 'cmdLen 0)     
    (hash-set! *iterator-table* 'vpointer 0)
    (hash-set! *iterator-table* 'count 0)

    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program)))
    (execute) ;start the actual execution of the code

)

(main (vector->list (current-command-line-arguments)))