;zxh108
;Zhenglin Huang
;optimized version, Part 2

(load "simpleParser.scm")

;takes a file and run the call tree generated from it
(define interpret
  (lambda (file)
    (run (parser file) newenvironment stdreturn stdreturn stdreturn)))

;returns a state that is modified by the call tree
;IN: queue is the list of statement, state is the state to operate on, 
;return is the function that takes our output state, when return is called 
;break is the function that takes output state when break is called
(define run
  (lambda (queue state return break continue)
    ;if the tree is empty
    (if (null? queue)
        ;then returns the result
        (return state)
        ;else we have different cases
        (cond
          ;if the first statement of the tree is null, then skip it, and execute the rest
          ((null? (car queue)) (run (cdr queue) state return break continue))
          ;else, let Mstate execute the current function. 
          ;((eq? (caar queue) 'if) (run (cons (if_replace (car queue) state) (cdr queue)) state return break continue))
          (else (Mstate-cps (car queue) state (lambda (v) (run (cdr queue) v return break continue)) break continue))))))

;(define if_replace
;  (lambda (statement state)
;    (if (Mboolean (condstmt statement) state)
;        (thenstmt statement)
;        (if (null? (cdddr statement))
;            '()
;            (elsestmt statement)))))
  
(define stdreturn
  (lambda (v) v))

(define Mstate-cps
  (lambda (statement state return break continue)
    (cond
      ;if the statement is empty, do nothing
      ((null? statement) (return state))
      ;if the first word is var, it is an declaration
      ((eq? 'var (operator statement)) (Mstate_declaration-cps statement state return))
      ;if the first word is "=", it is an assignment
      ((eq? '= (operator statement)) (Mstate_assignment-cps statement state return))
      ;if it starts with "return", we retrive the boolean or numerical value of the next item, the expression
      ((eq? 'return (operator statement)) (return (M_return statement state)))
      ;if it is an if statement
      ((eq? 'if (operator statement)) (Mstate_if-cps statement state return break continue))
      ;if it is a while loop
      ((eq? 'while (operator statement)) (M_while statement state return ))
      ;if we see a begin, then (cdr statement) is a tree
      ((eq? 'begin (operator statement)) (M_block_begin statement state return break continue))
      ((eq? 'continue (operator statement)) (continue state))
      ((eq? 'break (operator statement))  (break state))
      (else (error 'statement_not_recognized)))))

;takes a block-begin-end statement and run it
;returns the state in cps style
(define M_block_begin
  (lambda (statement state return break continue)
    (run 
     (cdr statement) 
     (enter_block state) 
     (lambda (result) (exit_block result return)) 
     (lambda (snapshot) (exit_block snapshot break)) 
     (lambda (snapshot) (exit_block snapshot continue))
     )))

;experimental implementation
(define M_while
  (lambda (statement state return)
         (M_while_loop statement state return return)))

;(define M_while
  ;(lambda (statement state return)
      ;(return (call/cc
       ;return here at any time we sees break, no more while execution
       ;(lambda (break)
         ;(M_while_loop statement state return break )))))) 
        
(define M_while_loop
  (lambda (statement state return break)
    (if (Mboolean (condstmt statement) state)
        (Mstate-cps 
         (thenstmt statement)
         state 
         (lambda (result_state) (M_while_loop statement result_state return break )) 
         break 
         (lambda (continue_state) (M_while_loop statement continue_state return break)))
        ;else
        (break state))))

(define M_return
  (lambda (statement state)
    ((lambda (print)
       (cond
         ((eq? print #t) 'true)
         ((eq? print #f) 'false)
         (else print)))
     (Mexpression (secondterm statement) state))))

;takes an if statement and a state, and generates a new state
;(define Mstate_if
 ; (lambda (statement state)
  ;  (Mstate_if-cps statement state stdreturn)))
  
(define Mstate_if-cps
  (lambda (statement state return break continue)
    ; if it is true
    (if (Mboolean (condstmt statement) state)
        ;do this as then
        (Mstate-cps (thenstmt statement) state return break continue)
        ;if it falls to else
        (if (null? (cdddr statement))
            (return state)  ;<-else does not exist, do nothing
            ;if else does exist as below-
            (Mstate-cps (elsestmt statement) state return break continue)))))

(define condstmt cadr)
(define thenstmt caddr)
(define elsestmt cadddr)

;takes an declaration and returns a state

(define Mstate_declaration
  (lambda (stmt state)
    (Mstate_declaration-cps stmt state stdreturn)))

(define Mstate_declaration-cps
  (lambda (statement state return)
    ;if it is a (var x) style
    (if (null? (aftersecondterm statement))
        ;adds a new one, not initialized. Remove the same name variable if there is any
        (return (add_declare_only (lhs statement) (remove_from_top_layer (lhs statement) state)))
        ;adds a new one with declared value. Removed the same variable previously first
        (return (add_with_assignment (lhs statement) (Mexpression (rhs statement) state) (remove_from_top_layer (lhs statement) state))))))

;takes an assignment statement
(define Mstate_assignment
  (lambda (stmt state)
    (Mstate_assignment-cps stmt state stdreturn)))

;cps
(define Mstate_assignment-cps
  (lambda (statement state return)
    ;if the variable is declared
    (return (update_var (lhs statement) (Mexpression (rhs statement) state) state))))

(define lhs cadr)
(define rhs caddr)

;takes an expression, and returns its value either in boolean or integer
;intact
(define Mexpression
  (lambda (expression state)
    (cond
      ((boolean? expression) (Mboolean expression state))
      ((number? expression) (Mvalue expression state))
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ((atom? expression) (Mexpression (lookup expression state) state))
      ;check for numerical calculation
      ((member? (operator expression) '(+ - * / %)) (Mvalue expression state))
      ;check for boolean calculation
      ((member? (operator expression) '(|| && ! == != > < <= >=)) (Mboolean expression state))
      ;if it is not boolean or numerical, we can't solve it for now
      (else (error 'expression_not_supported)))))

;retrieve the numerical value of an expression
;intact
(define Mvalue
  (lambda (expression state)
    (cond
      ((boolean? expression) (error 'type_incompatible_number_expected_but_boolean_given))
      ((number? expression) expression)
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ((atom? expression) (Mvalue (lookup_value expression state) state))
      ((eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state)
                                         (Mvalue (rightoperand expression) state)))
      ((eq? '- (operator expression)) (if (null? (cddr expression))
                                          (- 0 (Mvalue (leftoperand expression) state))
                                          (- (Mvalue (leftoperand expression) state)
                                             (Mvalue (rightoperand expression) state))))
      ((eq? '* (operator expression)) (* (Mvalue (leftoperand expression) state)
                                         (Mvalue (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state)
                                                (Mvalue (rightoperand expression) state)))
      ((eq? '% (operator expression)) (remainder (Mvalue (leftoperand expression) state)
                                                 (Mvalue (rightoperand expression) state)))
      (else (error 'bad_operator)))))

;retrieves a boolean number by executing the expression
;intact
(define Mboolean
  (lambda (expression state)
    (cond
      ((number? expression) (error 'type_incompatible_boolean_expected_but_number_given))
      ((boolean? expression) expression)
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ((atom? expression) (Mboolean (lookup expression state) state))
      ((eq? '|| (operator expression)) (if (Mboolean (leftoperand expression) state)
                                           true
                                           (Mboolean (rightoperand expression) state)))
      ((eq? '&& (operator expression)) (if (Mboolean (leftoperand expression) state) 
                                           (Mboolean (rightoperand expression) state)
                                           false))
      ((eq? '!  (operator expression)) (not (Mboolean (leftoperand expression) state)))
      ((eq? '== (operator expression)) (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))))
      ((eq? '>  (operator expression)) (> (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '<  (operator expression)) (< (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '<= (operator expression)) (<= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '>= (operator expression)) (>= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      (else (error 'bad-operator)))))



;abstractions for getting the three things right
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)
(define aftersecondterm cddr)
(define secondterm cadr)
(define thirdterm caddr)
(define atom? (lambda (v) (not (pair? v))))

;--------------Environment manipulations that is seperated from the code --------------
;Features of the environment
;	Add: takes a state and info to add, returns a state with binding added to top layer
;		Add_plain: add on a base layer
;	Remove: Remove can only remove the variable that is on the top layer
;		Remove_plain: preform removal on a plain layer without other layer on top of
;	Lookup:
;		Search top layer down, if found, immediately return to call/cc break. If not found, go to the the next layer.
;	Update: Lookup with a value intended. Once found, swap the box content.
;------ Layer tool -----

;IN: a state
;OUT: #t if a state has blocks inside
(define hasblock?
  (lambda (state)
    (not (null? (deletefirsttwo state)))))

;IP2 helper
(define deletefirstthree cdddr)
(define deletefirsttwo cddr)
(define inblock car)
(define outblock cdr)

;in: a state
;out: the input state with top layer removed
(define blow_top_layer
  (lambda (state)
    (blow_top_layer-cps state stdreturn)))

;in: a state
;out: the state with a new top layer added
(define enter_block
  (lambda (state)
    (cons newenvironment state)))

;in: a state, a return location
;out: the input state with top layer removed, feeded to the return function
;---CPS-----
(define blow_top_layer-cps
  (lambda (state return)
    (cond
      ((hasblock? state) (blow_top_layer-cps (inblock state) 
                                             (lambda (v) 
                                               (return (if (not v)
                                                   (outblock state)
                                                   (cons v (outblock state)))))))
      (else (return #f)))))

(define exit_block
  blow_top_layer-cps)

;in: a state
;out: the top layer of the state, if none, then the state itself
(define fetch_top_layer
  (lambda (state)
    (cond
      ((hasblock? state) (fetch_top_layer (inblock state)))
      (else state))))

;in: a replacement layer, and a state
;out: the state with top layer replaced by the replacement layer
;---CPS-----
(define switch_top_layer
  (lambda (replacement state)
    (switch_top_layer-cps replacement state stdreturn)))

(define switch_top_layer-cps
  (lambda (replacement state return)
    (cond
      ((hasblock? state) (switch_top_layer-cps replacement (inblock state) (lambda (v) (return (cons v (outblock state))))))
      (else (return replacement)))))

;------ Add ----------------------------
;method to add a new variable and assign its value, show that it is initialized
(define add_with_assignment
  (lambda (name value state)
    (add_to_top_layer name (box value) state)))

(define addvalue add_with_assignment)

;method to only declare a variable, show that it is initialized
(define add_declare_only
  (lambda (name state)
    (add_to_top_layer name (box '(null)) state)))

(define addvar add_declare_only)

;takes a layered state, and add the binding to the top layer
;Input: x1=name to be added x2=value too be added state
;Output: a state with such binding added to its top layer
(define add_to_top_layer
  (lambda (name value state)
    (switch_top_layer (add_without_block name value (fetch_top_layer state)) state)))

;takes a state without block, and add the binding to the state
;Input: x1=name to be added x2=value too be added, state to be modified
;Output: a state with such binding added

(define add_without_block
  (lambda (name value state)
    (combine (cons name (firstlist state)) (cons value (secondlist state)))))


;-------Remove ---------
;method to remove a variable from the an environment, and all attributes associated with it.

;remove a binding with name from top layer
;Input: a name for target removal, a state 
;Output: the targeted variable is removed from the top layer, if any
(define remove_from_top_layer
  (lambda (name state)
    (switch_top_layer (remove_without_block name (fetch_top_layer state)) state)))

(define remove_without_block
  (lambda (n s)
    (remove_without_block-cps n s stdreturn)))

;Input: a name, a state without blocks, and a return function
;Output: the state with binding corresponding to the name removed, if any
;---CPS-----
(define remove_without_block-cps
  (lambda (name state return)
    (cond
      ;nothing is left in the state, return the empty state
      ((null? (var_name_of state)) (return state))
      ;the first one is the target, remove it and return the rest
      ((eq? name (first_item (var_name_of state))) (return (remove_first state)))
      ;add the first back, after remove is performed at the rest
      (else (remove_without_block-cps name (remove_first state) (lambda (result) (return (add_without_block (first_item (var_name_of state)) (first_item (var_value_of state)) result))))))))

(define first_item car)
;------Lookup--------
;takes a name, a state
;intepret the box returned by the helper
; if the return box is '(null)
; the value to look up for is not assigned -> error
; if the return box look up returned a #f instead of a box
; the value to loop up for is not declared -> error
;---CPS cited-----
(define lookup_value
  (lambda (name state)
    ((lambda (box)
       (if (not box)
           (error 'notdeclared)
           ((lambda 
                (value)
                (if (and (pair? value) (eq? (car value) 'null))
                  (error 'notassigned)
                  value))
            (unbox box))))
     (lookup_box_general name state))))

(define lookup lookup_value)
;takes a name to look up for, and the state to loop up from
;returns the corresponding box from the inner most layer it was found
(define lookup_box_general
  (lambda (name2 state2)
    (call/cc
     (lambda (break)
       (letrec ((lookup_box (lambda (name state return)
                              (cond
                                ((hasblock? state) (if (not (lookup_box name (inblock state) stdreturn))
                                                       (lookup_box name (outblock state) stdreturn)
                                                       ))
                                (else 
                                 (if (member? name (var_name state))
                                     (break (find name (var_name state) (var_value state)))
                                     (return #f))))))) (lookup_box name2 state2 stdreturn))))))

;--------Update-------
;environment specific interfaces, strictly three lists and attributes are hard coded
(define update_var
  (lambda (name value state)
    ((lambda (box)
       (if (not box)
           (error 'notdeclared)
           (begin (set-box! box value) state)))
     (lookup_box_general name state))))

;the way to retrieve name, value and init? from the environment
(define v_name car)
(define var_name car)
(define firstlist car)
(define var_name_of car)
(define v_value cadr)
(define var_value cadr)
(define var_value_of cadr)
(define secondlist cadr)
(define v_init? caddr)


;find an item x from key(list), and return the corresponding item in the target(list)
(define find
  (lambda (x key target)
    (cond
      ((null? key) '(notfound))
      ((null? target) (error 'targetistooshort))
      ((eq? x (car key)) (car target))
      (else (find x (cdr key) (cdr target))))))

;create a state with 3 properties, empty
(define newenvironment '(()()))

;takes two sublists
;returns a state consists of 2 lists
(define combine
  (lambda (l1 l2)
    (cons l1 (cons l2 '()))))

;pass in a state with the first variable removed
(define remove_first
  (lambda (s)
    (combine (cdr (v_name s)) (cdr (v_value s)))))

;takes an atom and a list, to check whether the atom is in the list
(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))

