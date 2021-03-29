;GROUP 16: Abdasalaam Salem, Jamie Booker, Justin Galvez
#lang racket
(require "simpleParser.rkt")

;takes a filename that contains the code that is to be sent to the parser
(define interpret
  (lambda (filename)
    (evaluate-tree (parser filename))))

;evaluates the tree from interpret by calling evaluate-line
;a last car is added to the tree in case the last line is a while/if with a return statement inside
(define evaluate-tree
  (lambda (tree)
     (evaluate-line (car tree) (append (cdr tree) '((null))) initialstate)))

(define initialstate '((() ())))

;evaluates each line of the tree
;if the state is a number, this means there was a return statement inside an if/while statement
(define evaluate-line
  (lambda (line tree state)
    (cond
      ((and (boolean? state) (eq? state #t)) 'true)
      ((boolean? state) 'false)
      ((null? tree) state)
      ((number? state) state)
      (else (evaluate-line (car tree) (cdr tree) (M_state line state))))))
      
;if value of name is a non number/not a boolean, its undeclared
;line - the entire declaration expression
(define declaration
  (lambda (name line state)
    (if (null? (cddr line))
        (Add_M_state name 'null state)                                                  ;if variable name is declared without a value
        (Add_M_state name (M_value (caddr line) state) (M_state (caddr line) state))))) ;if name is declared with a value

;assigns variable name to value expression by first removing the variable and its old value from the state and adding it back in with the new value
(define assignment
  (lambda (name expression state)
    (if (layered_declare_check name state) ;if name has been declared
        (append (priorlist name state) (Add_M_state name (M_value expression state) (Remove_M_state name (M_state expression state)))) ;adds the name with the new value to the state with name removed
        (error "Not Declared"))))

(define priorlist
  (lambda (name state)
    (cond
      ((null? state) '())
      ((is_declared name (caar state)) '())
      (else (cons (car state) (priorlist name (cdr state)))))))

(define layered_declare_check
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((null? (car state)) #f)
      ((eq? (is_declared name (caar state)) #f) (layered_declare_check name (cdr state)))
      (else #t))))

;checks if the variable name has been declared before assignment
;gets the declare-list from the state (first sub list)
(define is_declared
  (lambda (name declare-list)
    (cond
      ((null? declare-list) #f)
      ((eq? name (car declare-list)) #t) ;if it finds the variable name in the declared list of variables, it is declared
      (else (is_declared name (cdr declare-list))))))

;determines if the variable has been assigned
;gets the declare-list from the state (first sub list) and the value-list from the state (second sublist)
;declare-list contains the variable names and value-list is their corresponding values
(define is_assigned
  (lambda (name declare-list value-list)
    (cond
      ((null? declare-list) error "Not Declared")
      ((eq? (car declare-list) name) (not (eq? (car value-list) 'null))) ;if the name is found in the declare-list and the value is not null, return true
      (else (is_assigned name (cdr declare-list) (cdr value-list))))))
      
(define get_from_layers
  (lambda (name state)
    (cond
      ((null? state) (error "Not Declared"))
      ((eq? (get_from_state name (caar state) (cadar state)) 'notdeclared) (get_from_layers name (cdr state)))
      ((eq? (get_from_state name (caar state) (cadar state)) 'notassigned) (error "Not Assigned"))
      (else (get_from_state name (caar state) (cadar state))))))

;reads through the declared/value list bindings and returns the value of the variable if found in declare-list and if it is assigned
(define get_from_state
  (lambda (name declare-list value-list)
    (cond
      ((null? declare-list) 'notdeclared)
      ((and (eq? name (car declare-list)) (eq? (car value-list) 'null)) 'notassigned) ;if the binding for variable name is null it is not assigned
      ((eq? name (car declare-list)) (car value-list))                                    ;returns value of name if it has been found
      (else (get_from_state name (cdr declare-list) (cdr value-list))))))

;returns the value of expression using the state and M_value function
(define return
  (lambda (expression state)
    (M_value expression (M_state expression state))))

;if condition is true, perform then-statement on the state
;line - entire if-then expression
(define if-statement
  (lambda (condition then-statement line state)
    (cond
      ((M_boolean condition (M_state condition state)) (M_state then-statement (M_state condition state))) ;if condition is true by M_boolean, perform then-statement with M_state
      ((null? (cdddr line)) (M_state condition state))
      (else (M_state (cadddr line) (M_state condition state))))))

;while condition is true, perform body statement on the state
(define while-statement
  (lambda (condition body-statement state)
    (if (M_boolean condition (M_state condition state))
        (while-statement condition body-statement (M_state body-statement (M_state condition state))) ;if condtion is true, run while statement again on the changed state
        (M_state condition state))))
        

;adds a variable and its value to state, if the value has been declared, but not assigned, its corresponding value is null
(define Add_M_state
  (lambda (name value state) ;car state = top layer caar state = top layer name's
    (cons (list (cons name (caar state)) (cons value (cadar state))) (cdr state))))  ;adds the variable by consing the new first layer with the cdr of state

;removes a variable and its corresponding value from the state by calling the remove function
(define Remove_M_state
  (lambda (name state)
    (cond
      ((null? state) '())
      ((is_declared name (caar state)) (cons (remove name (caar state) (cadar state) '() '()) (cdr state)))
      (else (Remove_M_state name (cdr state))))))


;helper function to remove a variable from state list, if variable isn't found then the original state is returned from a saved list
(define remove
  (lambda (name declare-list value-list saved-declare saved-value)
    (cond
      ((null? declare-list) (list saved-declare saved-value))
      ((eq? name (curr-value declare-list)) (list (append saved-declare (next-value declare-list)) (append saved-value (next-value value-list))))
      (else (remove name (next-value declare-list) (next-value value-list) (cons (curr-value declare-list) saved-declare) (cons (curr-value value-list) saved-value))))))

(define block
  (lambda (line state)
    (cond
      ((null? line) (remove_top state))
      (else (block (cdr line) (M_state (car line) state))))))
      
(define add_top
  (lambda (state)
    (cons '(() ()) state)))

(define remove_top cdr)

;curr-value gets the current value in a list
(define curr-value car)

;next-value gets the next values in a list
(define next-value cdr)

;reterns the state of an expression by calling on its respective function, otherwise the current state will be returned
(define M_state
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((not (list? expression)) state)
      ((list? (line-type expression)) (M_state (cdr expression) (M_state (car expression) state)))
      ((eq? (line-type expression) 'begin) (block (cdr expression) (add_top state)))
      ((eq? (line-type expression) 'return) (return (return-expression expression) state))
      ((eq? (line-type expression) 'var) (declaration (get-name expression) expression state))
      ((eq? (line-type expression) '=) (assignment (get-name expression) (get-expression expression) state))
      ((eq? (line-type expression) 'if) (if-statement (get-condition expression) (get-expression expression) expression state))
      ((eq? (line-type expression) 'while) (while-statement (get-condition expression) (get-expression expression) state))
      (else state))))

(define get-firstline cadr)

(define get-body caddr)

;gets the condition in a if or while statement
(define get-condition cadr)

;gets the then statement in a if or while statement
(define get-then-statement caddr)

;return-expression gets the return expression in a return statement
(define return-expression cadr)

;get-expression gets the expression in the assignment statement
(define get-expression caddr)

;get-name gets the variable name in a declaration/assignment statement
(define get-name cadr)

;line-type gets the type of the line, the first element in the line
(define line-type car)

;returns the boolean result of boolean and comparison operations 
;each operand will be sent to M_value for interpretation 
(define M_boolean
  (lambda (expression state)
    (cond
      ((not (list? expression)) (M_value expression state))
      ((eq? (operator expression) '&&) (and (M_boolean (leftoperand expression) state) (M_boolean (rightoperand expression) state)))
      ((eq? (operator expression) '||) (or (M_boolean (leftoperand expression) state) (M_boolean (rightoperand expression) state)))
      ((eq? (operator expression) '!) (not (M_boolean (leftoperand expression) state)))
      ((eq? (operator expression) '==) (= (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '!=) (not (= (M_value (leftoperand expression) state) (M_value (rightoperand expression) state))))
      ((eq? (operator expression) '<) (< (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '<=) (<= (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '>) (> (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '>=) (>= (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      (else (error "Invalid")))))

;gets the left operand of an expression
(define leftoperand cadr)

;gets the right operand of an expression
(define rightoperand caddr)

;operator gets the operator of an expression
(define operator car)

;returns the value of expression, whether it is a boolean, variable, arithmetic expression, or number
(define M_value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((not (pair? expression)) (get_from_layers expression state))
      ((eq? (operator expression) '+) (+ (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((and (eq? (operator expression) '-) (null? (cddr expression))) (- (M_value (leftoperand expression) state)))
      ((eq? (operator expression) '-) (- (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) 'var) (M_value (leftoperand expression) (M_state expression state)))
      ((eq? (operator expression) '=) (M_value (leftoperand expression) (M_state expression state)))
      (else (M_boolean expression state))))) ;if the value of expression is either a boolean test (like >=) or if the expression is invalid
