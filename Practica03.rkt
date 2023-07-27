#lang plai
;;Integrantes del Equipo
;; Marco Antonio Rivera Silva 318183583
;; Adrian Aguilera Moreno 421005200
;; Kevin Jair Torres Valencia 318331818
(define-type AST
  [id (i symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [op (f procedure?) (args (listof AST?))]
  [with (bindings (listof binding?)) (body AST?)]
  [with* (bindings (listof binding?)) (body AST?)]
  [fun (params (listof symbol?)) (body AST?)]
  [app (fun AST?) (args (listof AST?))])


(define-type Binding
  [binding (id symbol?) (value AST?)])

;; Ejercicio1
(define (parse sexp)
  (define (parse-op opsexp)
    (let([operador (case (first opsexp)
                     [(+) +]
                     [(-) -]
                     [(*) *]
                     [(/) /]
                     [(modulo) modulo]
                     [(expt) expt]
                     [(not) not])])
      (op operador (map parse (rest opsexp)))))
  ;; pasa listas tipo [a 2] a binding a (num 2)
  (define (toBinding lista)
    (cond
      [(number? (second lista)) (binding (first lista) (num (second lista)))]
      [(symbol? (first (second lista))) (binding (first lista) (parse (second lista)))]
      ))
  ;; aplica to binding a todas los elementos de una lista
  (define (toLB lista)
    (if (empty? lista)
        '()
        (cons (toBinding (first lista)) (toLB(rest lista)))))
  (cond
    [(symbol? sexp)
     (case sexp
       [(T) (bool #t)]
       [(F) (bool #f)]
       [(+ - * / modulo expt not with with* fun app) (error "No puedes usar id's reservados")]
       [else (id sexp)])]
    [(number? sexp) (num sexp)]
    [(list? sexp) (case (first sexp)
                    [(+ - * / modulo expt not) (parse-op sexp)]
                    [(with) (with (toLB (second sexp)) (parse (third sexp)))]
                    [(with*) (with* (toLB (second sexp)) (parse (third sexp)))]
                    [(fun) (fun (second sexp) (parse (third sexp)))]
                    [(app) (app (parse (second sexp)) (map parse (third sexp)))])]))


;; Ejercicio 2

(define (subst fwae-ast sub-id valor)

  ;;Checa los id de la lista
  (define (checar lista id)
    (if (empty? lista) #f
        (let ([aux (first lista)])
          (if (equal? (binding-id aux) id) #t
              (checar (rest lista) id)))))
  ;;Sustituye los Bindings
  (define (sustBind lista)
    #| La composición de funciones en las que aquí se invoca primero a <subst>, hace al intérprete <perezoso>.
     Esto se debe a que la sustitucion de los bindings debe realizarse antes de la interpretación|#
    (binding (binding-id lista) (subst (binding-value lista) sub-id valor)))
  (cond
    [(id? fwae-ast) (if (symbol=? (id-i sub-id) (id-i fwae-ast))
                        valor
                        fwae-ast)]
    [(op? fwae-ast) (op (op-f fwae-ast)
                        #| La composición de funciones en las que aquí se invoca primero a <subst>, hace al intérprete <perezoso>.
                        Esto se debe a que la sustitucion de los bindings debemos sustituir los valores
                        de la operación antes de la interpretación|#
                        (map (lambda (arg) (subst arg sub-id valor)) (op-args fwae-ast)))]
 
    [(with? fwae-ast) (if (checar (with-bindings fwae-ast) sub-id)
                          (with (map sustBind (with-bindings fwae-ast)) (with-body fwae-ast))
                          #| La composición de funciones en las que aquí se invoca primero a <subst>, hace al intérprete <perezoso>.
                          Esto se debe a que la sustitucion de los bindings del with debe realizarse antes de la interpretación|#
                          (with (map sustBind (with-bindings fwae-ast)) (subst (with-body fwae-ast) sub-id valor)))]
    [(with*? fwae-ast) (if (checar (with*-bindings fwae-ast) sub-id)
                           (with* (map sustBind (with*-bindings fwae-ast)) (with*-body fwae-ast))
                           #| La composición de funciones en las que aquí se invoca primero a <subst>, hace al intérprete <perezoso>.
                          Esto se debe a que la sustitucion de los bindings del with* debe realizarse antes de la interpretación|#
                           (with* (map sustBind (with*-bindings fwae-ast)) (subst (with*-body fwae-ast) sub-id valor)))]
    
    [(fun? fwae-ast) (if (member (id-i sub-id) (fun-params fwae-ast))
                         #| La composición de funciones en las que aquí se invoca primero a <subst>, hace al intérprete <perezoso>.
                          Esto se debe a que la sustitucion los parametros de la funcion deben realizarse antes de la interpretación|#
                         (subst (fun-body fwae-ast) sub-id valor)
                         fwae-ast)]
    #| La composición de funciones en las que aquí se invoca primero a <subst>, hace al intérprete <perezoso>.
    Esto se debe a que la sustitucion de los parametros de la funcion al igual que los parametros en el cuerpo de la funcion
    debe realizarse antes de la interpretación|#
    [(app? fwae-ast) (app (subst (app-fun fwae-ast) sub-id valor) (map (subst op sub-id valor) (app-args fwae-ast)))]
    [else fwae-ast])
  )







;; Ejercicio 3
(define (interp fwae-ast)
  (define (intW fwae lista)
    (if (empty? lista)
        fwae
        #| La composición de funciones en las que aquí se invoca primero a <subst>, hace al intérprete <perezoso>.
        Esto se debe a que la sustitucion de los valores de la operacion deben realizarse antes de la interpretación|#
        (intW (subst fwae (id (binding-id (first lista))) (binding-value (first lista))) (rest lista))))
  ;; FUncion auxiliar de interp para withs
  (define (intWBindings lista)
    (cond
      [(= 0 (length lista)) (error "No hay asignaciones")]
      [(= 1 (length lista)) lista]
      [else lista]))
    (cond
      [(id? fwae-ast) (error "error: Variable libre")]
      [(num? fwae-ast) (num-n fwae-ast)]
      [(bool? fwae-ast) (bool-b fwae-ast)]
      #| La composición de funciones en las que aquí se invoca primero a subst antes que interp, hace al intérprete <perezoso>.
        Esto se debe a que la sustitucion de los valores de la operacion deben realizarse antes de la interpretación, una vez
        sustituido procede a interpretar|#
      [(with? fwae-ast) (interp (intW (with-body fwae-ast) (with-bindings fwae-ast)))]
      [(with*? fwae-ast) (intWBindings (with*-bindings fwae-ast))]
      [(fun? fwae-ast) fwae-ast]
      #| La composición de funciones en las que aquí se invoca primero a interp, ya que en teoria ya deberia estar aplicada la
      sustitucion para aplicar la funcion|#
      [(op? fwae-ast) (apply (op-f fwae-ast) (map interp (op-args fwae-ast)))]
      [(app? fwae-ast) (cond
                         [(not (fun? (app-fun fwae-ast))) (error "No hay funcion que aplicar")]
                         #| La composición de funciones en las que aquí se invoca primero a interp, hace al intérprete <perezoso>.
                         Esto se debe a que la sustitucion de los valores de la operacion deben realizarse antes de la interpretación, una vez
                         sustituido procede a interpretar|#
                         [(= (length (fun-params (app-fun fwae-ast))) (length (app-args fwae-ast))) (interp (fun-body (app-fun fwae-ast)))]
                         [else (error "Numero de parametros no coincide")])]))


;; Pruebas Unitarias
;; Parse
(define (parse1)
  (test (parse 1) (num 1)))

(define (parse2)
  (test (parse 'a) (id 'a)))

(define (parse3)
  (test (parse '(+ 1 2)) (op + (list (num 1) (num 2)))))

(define (parse4)
  (test (parse '{with* {[a 2] [b {+ a a}]} b}) (with* (list (binding 'a (num 2)) (binding 'b (op + (list (id 'a) (id 'a))))) (id 'b))))

(define (parse5)
  (test (parse '(app (fun (a b c d) 1) ((+ 2 2) b c))) (app (fun '(a b c d) (num 1)) (list (op + (list (num 2) (num 2))) (id 'b) (id 'c)))))

(parse1)
(parse2)
(parse3)
(parse4)
(parse5)

;; Subst
(define (subst1)
  (test (subst (id 'a) (id 'a) (num 4)) (num 4)))

(define (subst2)
  (test (subst (fun (list 'a 'b) (id 'a)) (id 'a) (num 1)) (num 1)))

(define (subst3)
  (test (subst (op + (list (id 'a) (num 1))) (id 'a) (num 1)) (op + (list (num 1) (num 1)))))

(subst1)
(subst2)
(subst3)


;; Más Pruebas de Subst pero sin test jsjsjsjs
;;(subst (with (list (binding 'a (num 1))) (id 'b)) (id 'b) (num 1))
;;(subst (with (list (binding 'a (num 1))) (with (list (binding 'b (num 1))) (id 'c))) (id 'c) (num 2))
;;(subst (with (list (binding 'a (num 1)) (binding 'b (num 2))) (num 1) ) 'b (num 2))
;;(subst (op + (list (num 1) (num 1))) 'a 1)

;; Interp

;; (interp(parse 'foo))
;; devuelve error: Variable libre

;; (interp (parse 42))
;; devuelve 42

;; (interp (parse '{not T}))
;; devuelve false

;; (interp (parse '{with {[a 2] [b 3]} {+ a b}}))
;; devuelve 5

;; (interp (parse '{fun {a b} {expt a b}}))
;; devuelve (fun '(a b) (op #<procedure:expt> (list (id 'a) (id 'b))))

;; (interp (parse '(app (fun (a b c d) 1) ((+ 1 1) 1 1 1)))))
;; devuelve 1
