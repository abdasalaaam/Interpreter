#lang racket
;GROUP 16: Abdasalaam Salem, Jamie Booker, Justin Galvez
(require "functionParser.rkt")

;takes a filename that contains the code that is to be sent to the parser
(define interpret
  (lambda (filename)
    (evaluate-tree (parser filename))))

;evaluates the tree from interpret by calling evaluate-line
;a last car is added to the tree in case the last line is a while/if with a return statement inside
(define evaluate-tree
  (lambda (tree)
     (evaluate-line (car tree) (append (cdr tree) '((null))) initialstate)))

;initial state containing only one layer
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
      (else
       (call/cc
        (lambda (return)
       (evaluate-line (car tree) (cdr tree) (M_state line state '() '() '() return))))))))
      
;if value of name is a non number/not a boolean, its undeclared
;line - the entire declaration expression
(define declaration
  (lambda (name line state)
    (if (null? (cddr line))
        (Add_M_state name 'null state)                                                  ;if variable name is declared without a value
        (Add_M_state name (M_value (caddr line) state '() '() '() '()) (M_state (caddr line) state '() '() '() '()))))) ;if name is declared with a value

;assigns a new value to the box that is binded to name
(define assignment
  (lambda (name expression state break throw continue return)
    (if (layered_declare_check name state)
        (begin (set-box! (get_box_from_layers name state) (M_value expression state break throw continue return)) state)
        (error "Not Declared"))))

;goes through each layer, starting at the top, to check for variable declarations. Returns a scheme boolean
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

;gets the declarations list from a state
(define declarations-list caar)

;gets a values list from a state
(define values-list cadar)
      
;checks all layers, starting from top, for a name and returns its associated value or returns error if not assigned. used for acquiring the value of a box
(define get_from_layers
  (lambda (name state)
    (cond
      ((null? state) (error "Not Declared"))
      ((eq? (get_from_state name (declarations-list state) (values-list state)) 'notdeclared) (get_from_layers name (cdr state)))
      ((eq? (unbox (get_from_state name (declarations-list state) (values-list state))) 'null) (error "Not Assigned"))
      (else (get_from_state name (declarations-list state) (values-list state))))))

;checks all layers, starting from the top, and finds the box that is associated with name. used for acquiring the box.
(define get_box_from_layers
  (lambda (name state)
    (cond
      ((null? state) (error "Not Declared"))
      ((eq? (get_from_state name (declarations-list state) (values-list state)) 'notdeclared) (get_box_from_layers name (cdr state)))
      (else (get_from_state name (declarations-list state) (values-list state))))))
 
;reads through the declared/value list bindings and returns the value of the variable if found in declare-list and if it is assigned
(define get_from_state
  (lambda (name declare-list value-list)
    (cond
      ((null? declare-list) 'notdeclared)
      ((eq? name (car declare-list)) (car value-list))                             ;returns value of name if it has been found
      (else (get_from_state name (cdr declare-list) (cdr value-list))))))

;if condition is true, perform then-statement on the state
;line - entire if-then expression
(define if-statement
  (lambda (condition then-statement line state break throw continue return)
    (cond
      ((M_boolean condition (M_state condition state break throw continue return) break throw continue return) (M_state then-statement (M_state condition state break throw continue return) break throw continue return)) ;if condition is true by M_boolean, perform then-statement with M_state
      ((null? (cdddr line)) (M_state condition state break throw continue return))
      (else (M_state (cadddr line) (M_state condition state break throw continue return) break throw continue return)))))

;while condition is true, perform body statement on the state
(define while-statement
  (lambda (condition body-statement state throw return)
    (if (M_boolean condition (M_state condition state '() throw '() return) '() throw '() return)
        (call/cc
         (lambda (break)
        (while-statement condition body-statement
                         (call/cc (lambda (continue) (M_state body-statement (M_state condition state break throw continue return) break throw continue return))) throw return))) ;if condtion is true, run while statement again on the changed state
        (M_state condition state '() throw '() return))))
     
;adds a variable and its value to state, if the value has been declared, but not assigned, its corresponding value is null
(define Add_M_state
  (lambda (name value state) ;car state = top layer caar state = top layer name's
    (cons (list (cons name (caar state)) (cons (box value) (cadar state))) (cdr state))))  ;adds the variable by consing the new first layer with the cdr of state

;creates a function by adding it to the state with its associated closure
(define createfunc
  (lambda (name params body state)
    (Add_M_state name (cons params (cons body (cons state '()))) state)))

;calls a function by calling block on the closure with the function added to that specific closure
(define callfunc
  (lambda (name param state break throw continue return)
    (block (getbody name state) (addActParams (getFormalParams name state) param state (add_top (createfunc name (getFormalParams name state) (getbody name state) (getClosureState name state)))) break throw continue return)))

;adds the actual parameters to the formal parameters in the state
(define addActParams
  (lambda (formparams actparams OGstate state)
    (cond
      ((not (eq? (sizeOfParams formparams) (sizeOfParams actparams))) (error "Incorect number of function inputs"))
      ((null? formparams) state)
      ((null? actparams) state)
      ((Add_M_state (car formparams) (M_value (car actparams) OGstate '() '() '() '())  (addActParams (cdr formparams) (cdr actparams) OGstate state))))))

;returns the size of a list, particularly used for determining whether the parameter lists are the same size
(define sizeOfParams
  (lambda (lis)
    (cond
      ((null? lis) 0)
      (else (+ 1 (sizeOfParams (cdr lis)))))))

;gets the formal parameters for a function
(define getFormalParams
  (lambda (name state)
    (car (unbox (get_from_layers name state)))))

;gets the body for a function
(define getbody
  (lambda (name state)
    (cadr (unbox (get_from_layers name state)))))

;gets the closure for a function
(define getClosureState
  (lambda (name state)
    (caddr (unbox (get_from_layers name state)))))

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

;recursively reads and returns the MState of lines between brackets/ inside begin statements
(define block
  (lambda (line state break throw continue return)
    (cond
      ((null? line) (remove_top state))
      ((number? state) state)
      (else (block (cdr line) (M_state (car line) state break throw continue return) break throw continue return)))))
      
;simply adds a top layer to the state
(define add_top
  (lambda (state)
    (cons '(() ()) state)))

;removes the top layer from the state
(define remove_top
  (lambda (state)
    (if (not (list? state)) state (cdr state))))

;contains the conditions that a try block may encounter and calls the catch/finally/try-without-catch functions accordingly
(define try
  (lambda (line state break throw continue return)
    (cond
      ((not (number? (try_func (try-line line) state break throw continue return))) (try-without-catch line state break throw continue return))
      ((and (and (not (null? (catch-line line))) (not (null? (finally-line line))))) (finally (finally-line line) (catch (try_func (try-line line) state break throw continue return) (catch-line line) (add_top state) break throw continue return) break throw continue return))
      ((and (not (null? (catch-line line))) (null? (finally-line line))) (catch (try_func (try-line line) state break throw continue return) (catch-line line) (add_top state) break throw continue return))
      ((and (null? (catch-line line)) (not (null? (finally-line line)))) (finally (finally-line line) (try_func (try-line line) state break throw continue return) break throw continue return))
      ((and (null? (catch-line line)) (null? (finally-line line))) (try_func (try-line line) state break throw continue return))
      (else state))))

;A try without a catch will either only run the try block or run the try block and continue on to the final
(define try-without-catch
  (lambda (line state break throw continue return)
    (cond
      ((not (null? (finally-line line))) (finally (finally-line line) (try_func (try-line line) state break throw continue return) break throw continue return))
      (else (try_func (try-line line) state break throw continue return)))))

;assess the M_state of the try block function
(define try_func
  (lambda (line state break throw continue return)
    (call/cc
     (lambda (throw)
       (block line (add_top state) break throw continue return)))))

;Evaluates the body of catch blocks by calling the block function
(define catch
  (lambda (throw_value line state break throw continue return)
    (block (catch-body line) (Add_M_state (input_param line) throw_value state) break throw continue return)))

;Evaluates the finally block by calling the block function
(define finally
  (lambda (line state break throw continue return)
    (block (finally-body line) (add_top state) break throw continue return)))

;entire catch line containing "catch"
(define catch-line cadr)

;body for finally in the finally statement
(define finally-body cadr)

;body for catch in the catch statement
(define catch-body caddr)

;entire finally line containing "finally"
(define finally-line caddr)

;the (e) in catch (e)
(define input_param caadr)

;body for the try statement
(define try-line car)

;curr-value gets the current value in a list
(define curr-value car)

;next-value gets the next values in a list
(define next-value cdr)

(define main-check cadr)

;gets the parameters from a closure
(define get-params cadr)

;gets the body from a closure
(define get-body caddr)

;gets the environment from a closure
(define get-environment cadddr)

;reterns the state of an expression by calling on its respective function, otherwise the current state will be returned
(define M_state
  (lambda (expression state break throw continue return)
    (cond
      ((null? expression) state)
      ((not (list? expression)) state)
      ((list? (line-type expression)) (M_state (cdr expression) (M_state (car expression) state break throw continue return) break throw continue return))
      ((eq? (line-type expression) 'begin) (block (cdr expression) (add_top state) break throw continue return))
      ((and (eq? (line-type expression) 'function) (eq? (main-check expression) 'main)) (block (cdddr expression) (add_top state) break throw continue return))
      ((eq? (line-type expression) 'return) (return (M_value (return-expression expression) state break throw continue return)))
      ((eq? (line-type expression) 'var) (declaration (get-name expression) expression state))
      ((eq? (line-type expression) '=) (assignment (get-name expression) (get-expression expression) state break throw continue return))
      ((eq? (line-type expression) 'if) (if-statement (get-condition expression) (get-expression expression) expression state break throw continue return))
      ((eq? (line-type expression) 'while) (while-statement (get-condition expression) (get-expression expression) state throw return))
      ((and (eq? (line-type expression) 'break) (eq? break '())) (error "break not inside loop"))
      ((and (eq? (line-type expression) 'throw) (eq? throw '())) (error "throw not inside try"))
      ((eq? (line-type expression) 'break) (break (remove_top state)))
      ((eq? (line-type expression) 'try) (try (cdr expression) state break throw continue return))
      ((eq? (line-type expression) 'throw) (throw (M_value (cadr expression) state break throw continue return)))
      ((eq? (line-type expression) 'continue) (continue (remove_top state)))
      ((eq? (line-type expression) 'function) (createfunc (get-params expression) (get-body expression) (get-environment expression) state))
      ((eq? (line-type expression) 'funcall) (begin (call/cc
                     (lambda (return) (callfunc (get-params expression) (cddr expression) state break throw continue return))) state)) ;paused here - working on creating a parameter function
      (else state))))

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
  (lambda (expression state break throw continue return)
    (cond
      ((not (list? expression)) (M_value expression state break throw continue))
      ((eq? (operator expression) '&&) (and (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      ((eq? (operator expression) '||) (or (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      ((eq? (operator expression) '!) (not (M_value (leftoperand expression) state break throw continue return)))
      ((eq? (operator expression) '==) (= (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      ((eq? (operator expression) '!=) (not (= (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return))))
      ((eq? (operator expression) '<) (< (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      ((eq? (operator expression) '<=) (<= (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      ((eq? (operator expression) '>) (> (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      ((eq? (operator expression) '>=) (>= (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      (else (error "Invalid")))))

;gets the left operand of an expression
(define leftoperand cadr)

;gets the right operand of an expression
(define rightoperand caddr)

;operator gets the operator of an expression
(define operator car)

;returns the value of expression, whether it is a boolean, variable, arithmetic expression, or number
(define M_value
  (lambda (expression state break throw continue return)
    (cond
      ((number? expression) expression)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((not (pair? expression)) (unbox (get_from_layers expression state)))
      ((eq? (operator expression) '+) (+ (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      ((eq? (operator expression) '/) (quotient (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      ((eq? (operator expression) '%) (remainder (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      ((and (eq? (operator expression) '-) (null? (cddr expression))) (- (M_value (leftoperand expression) state break throw continue return)))
      ((eq? (operator expression) '-) (- (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      ((eq? (operator expression) '*) (* (M_value (leftoperand expression) state break throw continue return) (M_value (rightoperand expression) state break throw continue return)))
      ((eq? (operator expression) 'var) (M_value (leftoperand expression) (M_state expression state '()) break throw continue return))
      ((eq? (operator expression) '=) (M_value (leftoperand expression) (M_state expression state '()) break throw continue return))
      ((eq? (operator expression) 'funcall) (call/cc
                                             (lambda (return) (callfunc (cadr expression) (cddr expression) state break throw continue return))))
      (else (M_boolean expression state break throw continue return))))) ;if the value of expression is either a boolean test (like >=) or if the expression is invalid
