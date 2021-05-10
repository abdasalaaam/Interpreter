#lang racket
;GROUP 16: Abdasalaam Salem, Jamie Booker, Justin Galvez
(require "classParser.rkt")

;takes a filename that contains the code that is to be sent to the parser
(define interpret
  (lambda (filename classname)
    (evaluate-tree (parser filename) (string->symbol classname))))

;evaluates the tree from interpret by calling evaluate-line
;a last car is added to the tree in case the last line is a while/if with a return statement inside
(define evaluate-tree
  (lambda (tree classname)
     (evaluate-line (car tree) (append (cdr tree) '((null))) initialstate classname)))

;initial state containing only one layer
(define initialstate '((() ())))

;evaluates each line of the tree
;if the state is a number, this means there was a return statement inside an if/while statement
(define evaluate-line
  (lambda (line tree state classname)
    (cond
      ((and (boolean? state) (eq? state #t)) 'true)
      ((boolean? state) 'false)
      ((null? tree) (call/cc (lambda (return) (runmain classname state return))))
      ((number? state) state)
      (else
       (call/cc
        (lambda (return)
       (evaluate-line (car tree) (cdr tree) (M_state line state '() '() '() return '()) classname)))))))

;runs the main function by finding the static method main within the class in question
(define runmain
  (lambda (classname state return)
    (block (cadr (getStaticMethod 'main classname state)) (add_top state) '() '() '() return classname)))
    
;creates instance closures in the order: instance class > instance field values
(define makeInstanceClosure
  (lambda (line state)
    (list (cdr line) (cadr (unbox (get_from_layers (cadr line) state))))))

;Adds a class binded to its closure to the M_state by using the classClosure helper class
(define create_class
  (lambda (name line state)
    (Add_M_state name (makeClassClosure line) state)))

;returns the class body used for finding the m_state calls within a class
(define class-body cadddr)

;creates class closures in the order: super class > fields & values > methods > static methods
(define makeClassClosure
  (lambda (body)
    (list (pullSuper body) (findItems 'var (class-body body) initialstate) (findItems 'function (class-body body) initialstate) (findItems 'static-function (class-body body) initialstate))))

;Helper function to pull the keywords out of class bodies: 'var, 'function, 'static-function
;used in defining the class closure. searches through the class body and locates all M_state instantiations - variables or nonstatic/static methods
;then adds those instantiations into an empty state for creation of many states within the class closure.
(define findItems
  (lambda (item body state)
    (cond
      ((null? body) state)
      ((eq? (caar body) item) (findItems item (cdr body) (M_state (car body) state '() '() '() '() '())))
      (else (findItems item (cdr body) state)))))

;Pulls the super class from a class definition
(define pullSuper
  (lambda (classline)
    (cond
      ((null? classline)           '())
      ((null? (car classline))     '())
      ((and (list? (car classline)) (eq? (caar classline) 'extends))  (cons (cdar classline) '()))
      (else                        (pullSuper (cdr classline))))))

;returns a static method's closure from within the class closure. used in calling the main function.
(define getStaticMethod
  (lambda (name classname state)
    (unbox (get_from_layers name (cadddr (unbox (get_from_layers classname state)))))))

;returns a nonstatic method's closure from within the class closure
(define getNonStaticMethod
  (lambda (name classname state)
    (unbox (get_from_layers name (caddr (unbox (get_from_layers classname state)))))))

;if value of name is a non number/not a boolean, its undeclared
;line - the entire declaration expression
(define declaration
  (lambda (name line state current)
    (if (null? (cddr line))
        (Add_M_state name 'null state)                                                  ;if variable name is declared without a value
        (Add_M_state name (M_value (caddr line) state '() '() '() '() current) (M_state (caddr line) state '() '() '() '() current))))) ;if name is declared with a value

;assigns a new value to the box that is binded to name
(define assignment
  (lambda (name expression state break throw continue return current)
    (cond
      ((pair? name) (begin (set-box!(get_box_from_layers (checkThis name current) state) (update-box (M_value expression state break throw continue return current) (unbox (get_from_layers (checkThis name current) state)) expression)) state))
      ((layered_declare_check name state)                        (begin (set-box!(get_box_from_layers name state) (M_value expression state break throw continue return current)) state))
      (else (error "Not Declared")))))

;helper used to check if the class name in a dot operator is this or a regular name
(define checkThis
  (lambda (name current)
    (if (eq? (cadr name) 'this) current
        (cadr name))))

;updates the box in an instance closure for assigning variables that belong to instances. gives a problem since we are using boxes
;the box that is represented by the variable in the instance closure is the same box for all instances that use the same name variable
(define update-box
  (lambda (value closure varname)
    (list (car closure) (assignment varname value (cadr closure) '() '() '() '() '()))))

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
  (lambda (condition then-statement line state break throw continue return current)
    (cond
      ((M_boolean condition (M_state condition state break throw continue return current) break throw continue return) (M_state then-statement (M_state condition state break throw continue return current) break throw continue return current)) ;if condition is true by M_boolean, perform then-statement with M_state
      ((null? (cdddr line)) (M_state condition state break throw continue return current current))
      (else (M_state (cadddr line) (M_state condition state break throw continue return current) break throw continue return current)))))

;while condition is true, perform body statement on the state
(define while-statement
  (lambda (condition body-statement state throw return current)
    (if (M_boolean condition (M_state condition state '() throw '() return current) '() throw '() return current)
        (call/cc
         (lambda (break)
        (while-statement condition body-statement
                         (call/cc (lambda (continue) (M_state body-statement (M_state condition state break throw continue return current) break throw continue return current))) throw return current))) ;if condtion is true, run while statement again on the changed state
        (M_state condition state '() throw '() return current))))
     
;adds a variable and its value to state, if the value has been declared, but not assigned, its corresponding value is null
(define Add_M_state
  (lambda (name value state) ;car state = top layer caar state = top layer name's
    (cons (list (cons name (caar state)) (cons (box value) (cadar state))) (cdr state))))  ;adds the variable by consing the new first layer with the cdr of state

;creates a function by adding it to the state with its associated closure
(define createfunc
  (lambda (name params body state class)
    (Add_M_state name (cons params (cons body (cons state (cons class '())))) state)))

;calls a function by calling block on the closure with the function added to that specific closure
(define callfunc
  (lambda (name param state break throw continue return current)
    (block (findClassMethodBody name state (checkCurrent current name))
           (addActParams (findMethodParams name state (checkCurrent current name)) param state
                         (add_top (Add_M_state current (unbox (get_from_layers current state))
                                               (createfunc (getDotName name) (findMethodParams name state (checkCurrent current name))
                                                                                  (findClassMethodBody name state (checkCurrent current name))
                                                                                  (findClosureState name state (checkCurrent current name))
                                                                                  (findMethodClass name state (checkCurrent current name))))))
           break throw continue return current)))
           
;helper method used to differentiate between the class that a function is defined in and the class that resembles this on the left side of a dot operator
(define checkCurrent
  (lambda (outsideClass functionToCall)
    (if (not (pair? functionToCall)) outsideClass
        (cadr functionToCall))))
  
(define getDotName
  (lambda (name)
    (if (not (pair? name)) name
        (caddr name))))

;finds the parameters for a method that was defined in a class. searches through class closure and finds method closure to do so
(define findMethodParams
  (lambda (expression state current)
    (cond
      ((not (pair? expression)) (getFormalParams expression state))
      ((eq? (car expression) 'dot) (car (getNonStaticMethod (caddr expression) (caar (getInstanceClosure (cadr expression) state current)) state)))
      (else expression))))

;finds the body for a method that was defined in a class
(define findClassMethodBody
  (lambda (expression state current)
    (cond
      ((not (pair? expression)) (getbody expression state))
      ((eq? (car expression) 'dot) (cadr (getNonStaticMethod (caddr expression) (caar (getInstanceClosure (cadr expression) state current)) state)))
      (else expression))))

;finds the state that was at function definition for a function that was defined in a class
(define findClosureState
  (lambda (expression state current)
    (cond
      ((not (pair? expression)) (getClosureState expression state))
      ((eq? (car expression) 'dot) (caddr (getNonStaticMethod (caddr expression) (caar (getInstanceClosure (cadr expression) state current)) state)))
      (else expression))))

;finds the class that the specific method is defined in
(define findMethodClass
  (lambda (expression state current)
    (cond
      ((not (pair? expression)) (getMethodClass expression state))
      ((eq? (car expression) 'dot) (cadddr (getNonStaticMethod (caddr expression) (caar (getInstanceClosure (cadr expression) state current)) state)))
      (else expression))))
  
;adds the actual parameters to the formal parameters in the state
(define addActParams
  (lambda (formparams actparams OGstate state)
    (cond
      ((not (eq? (sizeOfParams formparams) (sizeOfParams actparams))) (error "Incorect number of function inputs"))
      ((null? formparams) state)
      ((null? actparams) state)
      ((Add_M_state (car formparams) (M_value (car actparams) OGstate '() '() '() '() '())  (addActParams (cdr formparams) (cdr actparams) OGstate state))))))

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

;gets the class that the method is defined in
(define getMethodClass
  (lambda (name state)
    (cadddr (unbox (get_from_layers name state)))))

;recursively reads and returns the MState of lines between brackets/ inside begin statements
(define block
  (lambda (line state break throw continue return this)
    (cond
      ((null? line) (remove_top state))
      ((number? state) state)
      (else (block (cdr line) (M_state (car line) state break throw continue return this) break throw continue return this)))))
      
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
  (lambda (line state break throw continue return current)
    (cond
      ((not (number? (try_func (try-line line) state break throw continue return current))) (try-without-catch line state break throw continue return current))
      ((and (and (not (null? (catch-line line))) (not (null? (finally-line line))))) (finally (finally-line line) (catch (try_func (try-line line) state break throw continue return current) (catch-line line) (add_top state) break throw continue return current) break throw continue return current))
      ((and (not (null? (catch-line line))) (null? (finally-line line))) (catch (try_func (try-line line) state break throw continue return current) (catch-line line) (add_top state) break throw continue return current))
      ((and (null? (catch-line line)) (not (null? (finally-line line)))) (finally (finally-line line) (try_func (try-line line) state break throw continue return current) break throw continue return current))
      ((and (null? (catch-line line)) (null? (finally-line line))) (try_func (try-line line) state break throw continue return current))
      (else state))))

;A try without a catch will either only run the try block or run the try block and continue on to the final
(define try-without-catch
  (lambda (line state break throw continue return current)
    (cond
      ((not (null? (finally-line line))) (finally (finally-line line) (try_func (try-line line) state break throw continue return) break throw continue return))
      (else (try_func (try-line line) state break throw continue return current)))))

;assess the M_state of the try block function
(define try_func
  (lambda (line state break throw continue return current)
    (call/cc
     (lambda (throw)
       (block line (add_top state) break throw continue return current)))))

;Evaluates the body of catch blocks by calling the block function
(define catch
  (lambda (throw_value line state break throw continue return current)
    (block (catch-body line) (Add_M_state (input_param line) throw_value state) break throw continue return current)))

;Evaluates the finally block by calling the block function
(define finally
  (lambda (line state break throw continue return current)
    (block (finally-body line) (add_top state) break throw continue return current)))

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
  (lambda (expression state break throw continue return current)
    (cond
      ((null? expression) state)
      ((not (list? expression)) state)
      ((list? (line-type expression)) (M_state (cdr expression) (M_state (car expression) state break throw continue return current) break throw continue return current))
      ((eq? (line-type expression) 'begin) (block (cdr expression) (add_top state) break throw continue return current))
      ((eq? (line-type expression) 'return) (return (M_value (return-expression expression) state break throw continue return current)))
      ((eq? (line-type expression) 'var) (declaration (get-name expression) expression state current))
      ((eq? (line-type expression) '=) (assignment (get-name expression) (get-expression expression) state break throw continue return current))
      ((eq? (line-type expression) 'if) (if-statement (get-condition expression) (get-expression expression) expression state break throw continue return))
      ((eq? (line-type expression) 'while) (while-statement (get-condition expression) (get-expression expression) state throw return))
      ((and (eq? (line-type expression) 'break) (eq? break '())) (error "break not inside loop"))
      ((and (eq? (line-type expression) 'throw) (eq? throw '())) (error "throw not inside try"))
      ((eq? (line-type expression) 'break) (break (remove_top state)))
      ((eq? (line-type expression) 'try) (try (cdr expression) state break throw continue return current))
      ((eq? (line-type expression) 'throw) (throw (M_value (cadr expression) state break throw continue return current)))
      ((eq? (line-type expression) 'continue) (continue (remove_top state)))
      ((eq? (line-type expression) 'function) (createfunc (get-params expression) (get-body expression) (get-environment expression) state current))
      ((eq? (line-type expression) 'static-function) (createfunc (get-params expression) (get-body expression) (get-environment expression) state current))
      ((eq? (line-type expression) 'funcall) (begin (call/cc
                     (lambda (return) (callfunc (cadr expression) (cddr expression) state break throw continue return (checkCurrent current (cadr expression))))) state)) ;paused here - working on creating a parameter function
      ((eq? (line-type expression) 'class) (create_class (cadr expression) expression state))
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
  (lambda (expression state break throw continue return current)
    (cond
      ((not (list? expression)) (M_value expression state break throw continue current))
      ((eq? (operator expression) '&&) (and (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      ((eq? (operator expression) '||) (or (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      ((eq? (operator expression) '!) (not (M_value (leftoperand expression) state break throw continue return current)))
      ((eq? (operator expression) '==) (= (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      ((eq? (operator expression) '!=) (not (= (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current))))
      ((eq? (operator expression) '<) (< (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      ((eq? (operator expression) '<=) (<= (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      ((eq? (operator expression) '>) (> (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      ((eq? (operator expression) '>=) (>= (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      (else (error "Invalid")))))

;gets the left operand of an expression
(define leftoperand cadr)

;gets the right operand of an expression
(define rightoperand caddr)

;operator gets the operator of an expression
(define operator car)

;returns the value of expression, whether it is a boolean, variable, arithmetic expression, or number
(define M_value
  (lambda (expression state break throw continue return current)
    (cond
      ((number? expression) expression)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((not (pair? expression))             (unbox (get_from_layers expression state)))
      ((eq? (operator expression) '+)       (+ (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      ((eq? (operator expression) '/)       (quotient (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      ((eq? (operator expression) '%)       (remainder (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      ((and (eq? (operator expression) '-)  (null? (cddr expression))) (- (M_value (leftoperand expression) state break throw continue return current)))
      ((eq? (operator expression) '-)       (- (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      ((eq? (operator expression) '*)       (* (M_value (leftoperand expression) state break throw continue return current) (M_value (rightoperand expression) state break throw continue return current)))
      ((eq? (operator expression) 'var)     (M_value (leftoperand expression) (M_state expression state '() '() '() '() current) break throw continue return current))
      ((eq? (operator expression) '=)       (M_value (leftoperand expression) (M_state expression state '() '() '() '() current) break throw continue return current))
      ((eq? (operator expression) 'funcall) (call/cc
                                            (lambda (return) (callfunc (cadr expression) (cddr expression) state break throw continue return (checkCurrent current (cadr expression))))))
      ((eq? (operator expression) 'new)     (makeInstanceClosure expression state))
      ((eq? (operator expression) 'dot)     (M_value (rightoperand expression) (cadr (getInstanceClosure (leftoperand expression) state current)) break throw continue return current))
      (else (M_boolean expression state break throw continue return current))))) ;if the value of expression is either a boolean test (like >=) or if the expression is invalid
  
;retunrs the instance closure for a certain instance
(define getInstanceClosure
  (lambda (class state current)
    (if (eq? class 'this) (unbox (get_from_layers current state))
      (unbox (get_from_layers class state)))))
