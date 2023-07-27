#lang plai
;;Integrantes del Equipo
;; Marco Antonio Rivera Silva 318183583
;; Adrian Aguilera Moreno 421005200
;; Kevin Jair Torres Valencia 318331818
(define-type AST
  [id (i symbol?)] ;; listo
  [num (n number?)] ;; listo
  [bool (b boolean?)] ;; listo
  [op (f procedure?) (args (listof AST?))] ;; listo
  [op-bool (f symbol?) (larg AST?) (rarg AST?)] ;; listo
  [with (bindings (listof binding?)) (body AST?)] ;; listo
  [with* (bindings (listof binding?)) (body AST?)] ;; listo
  [fun (params (listof symbol?)) (body AST?)] ;; listo
  [lempty] ;; listo
  [lcons (l AST?) (r AST?)] ;; listo
  [lcar (lst AST?)] ;; listo
  [lcdr (lst AST?)] ;; listo
  [app (fun AST?) (args (listof AST?))]) ;; listo

(define-type Binding
  [binding (id symbol?) (value AST?)])

(define-type Environment
  [mtSub]
  [aSub (name symbol?) (value AST?) (bSub Environment?)])

(define-type FWAEL-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [listV (l (listof FWAEL-Value?))]
  [closureV (param (listof symbol?)) (body AST?) (env Environment?)])


;; Funcion Parse
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
    ;; Atomicas
    [(symbol? sexp)
     (case sexp
       [(T) (bool #t)]
       [(F) (bool #f)]
       [(lempty) (lempty)]
       [(+ - * / modulo expt not with with* fun app) (error "No puedes usar palabras reservadas")]
       [else (id sexp)])]
    [(number? sexp) (num sexp)]
    ;; Complejas
    [(list? sexp) (case (first sexp)
                    [(+ - * / modulo expt not) (parse-op sexp)]
                    [(and) (and (second sexp))]
                    [(or) (or (second sexp))]
                    [(op-bool) (op-bool (second sexp) (parse (third sexp)) (parse (fourth sexp)))]
                    [(with) (with (toLB (second sexp)) (parse (third sexp)))]
                    [(with*) (with* (toLB (second sexp)) (parse (third sexp)))]
                    [(fun) (fun (second sexp) (parse (third sexp)))]
                    [(lempty) (lempty)]
                    [(lcons) (lcons (parse (second sexp)) (parse (third sexp)))]
                    [(lcar) (lcar (parse (second sexp)))]
                    [(lcdr) (lcdr (parse (second sexp)))]
                    [(app) (app (parse (second sexp)) (map parse (third sexp)))])]))

;; Funcion Desugar
(define (desugar s-fwael-expr)
  ;; Quita azucar de with
  (define (desugar-with wexpr)
        ; Los WITH deben ser transformados en APP
        ; NOTA - APP también posee azúcar sintáctica
        (let (
            [bindings (with-bindings wexpr)])
            (app
                (fun (map (lambda (bdg) (binding-id bdg)) bindings)
                    (with-body wexpr))
                (map (lambda (bdg) (binding-value bdg)) bindings))))
  ;; Quita azucar de with*
  (define (desugar-with* wexpr)
    (let (
          [bindings (with*-bindings wexpr)]
          [body (with*-body wexpr)])
      (with (map (lambda (bdg) (desugar (binding-value bdg))) (list (first bindings)))
            (if (empty? (rest bindings))
                body
                (desugar-with* (with* (rest bindings) body))))))
  ;; Quita azucar de fun
  (define (desugar-fun funcion)
    (let (
          [params (fun-params funcion)])
      (fun (first params)
           (if (empty? (rest params))
               (fun-body funcion)
               (desugar-fun (fun (rest params) (fun-body funcion)))))))
  ;; Quita azucar de app
  (define (desugar-app app-expr)
    (let (
          [realp (app-args app-expr)]
          [formalp (fun-params (app-fun app-expr))]
          [body (fun-body (app-fun app-expr))])
      (app (fun (list (first formalp))
                (if (empty? (rest formalp))
                    body
                    (desugar-app (app (fun (rest formalp) body) (rest realp))))
                (map (lambda (param) (desugar param)) (list (first realp)))))))
  (cond
    ;; Estevia (Sin azucar)
    [(id? s-fwael-expr) s-fwael-expr]
    [(num? s-fwael-expr) s-fwael-expr]
    [(bool? s-fwael-expr) s-fwael-expr]
    [(lempty? s-fwael-expr) s-fwael-expr]
    ;; Saben a azucar, son de azucar, pero no son azucar xd
    [(op? s-fwael-expr) (op (op-f s-fwael-expr) (map (lambda (arg) (desugar arg)) (op-args s-fwael-expr)))]
    [(op-bool? s-fwael-expr) (op-bool (op-bool-f s-fwael-expr) (desugar (op-bool-larg s-fwael-expr)) (desugar (op-bool-rarg s-fwael-expr)))]
    [(lcons? s-fwael-expr) (lcons (desugar (lcons-l s-fwael-expr)) (desugar (lcons-r s-fwael-expr)))]
    [(lcar? s-fwael-expr) (lcar (desugar (lcar-lst s-fwael-expr)))]
    [(lcdr? s-fwael-expr) (lcdr (desugar (lcdr-lst s-fwael-expr)))]
    ;; Tienen Azucaaaa
    [(with? s-fwael-expr) (desugar (desugar-with s-fwael-expr))]
    [(with*? s-fwael-expr) (desugar (desugar-with* s-fwael-expr))]
    [(fun? s-fwael-expr) (desugar (desugar-fun s-fwael-expr))]
    [(app? s-fwael-expr) (desugar (desugar-app s-fwael-expr))]))

;; Subs de las funciones
(define (subst fwael-expr sub-id env)
  ;; Busca el id
  (define (find-inenv env)
        (if (mtSub? env)
            (error "Error" sub-id "es una variable libre")
            (if (eq? (aSub-name env) sub-id)
                (aSub-value env)
                (find-inenv (aSub-bSub env)))))
  (cond
    ;; Busca id, si lo encuentra, lo sustituye, si no, manda error
    [(id? fwael-expr) (if (eq? sub-id (id-i fwael-expr))
            (find-inenv env)
            fwael-expr)]
    ;; No sustituye nada porque no hay nada que sustituir
    [(num? fwael-expr) fwael-expr]
    ;; No sustituye nada porque no hay nada que sustituir
    [(bool? fwael-expr) fwael-expr]
    ;; No sustituye nada porque no hay nada que sustituir
    [(lempty? fwael-expr) fwael-expr]
    ;; Sustituye en sus argumentos
    [(op? fwael-expr) (op (op-f fwael-expr) (map (lambda (arg) (subst arg sub-id env))))]
    ;; Sustituye en su lado izquierdo y derecho
    [(op-bool? fwael-expr) (op-bool (op-bool-f fwael-expr) (subst (op-bool-larg fwael-expr) sub-id env) (subst (op-bool-rarg fwael-expr) sub-id env))]
    ;; Sustituye en sus ambos lados de la lista
    [(lcons? fwael-expr) (lcons (subst (lcons-l fwael-expr) sub-id env) (subst (lcons-r fwael-expr) sub-id env))]
    ;; Sustituye en su lista
    [(lcar? fwael-expr) (lcar (subst (lcar-lst fwael-expr) sub-id env))]
    ;; Sustituye en su lista
    [(lcdr? fwael-expr) (lcdr (subst (lcdr-lst fwael-expr) sub-id env))]
    ;; Sustituye en el resultado del desugar
    [(with? fwael-expr) (subst (desugar fwael-expr) sub-id env)]
    ;; Sustituye en el resultado del desugar
    [(with*? fwael-expr) (subst (desugar fwael-expr) sub-id env)]
    ;; Sustituye en el parametro de la funcion
    [(fun? fwael-expr) (subst (fun-body fwael-expr) (first (fun-params fwael-expr)) env)]
    ;; Sustituye en la aplicacion de la funcion
    [(app? fwael-expr) (let (
                             [function (app-fun fwael-expr)])
                         (app (fun (fun-params function) (subst (fun-body function) env) (subst (app-args fwael-expr) env))))]))

;; Interp
(define (interp fwael-expr env)
  ;; Busca el id
  (define (find-inenv env)
        (if (mtSub? env)
            (error "Error" (id-i fwael-expr) "es una variable libre")
            (if (eq? (aSub-name env) (id-i fwael-expr))
                (aSub-value env)
                (find-inenv (aSub-bSub env)))))
  (cond
    ;; Busca el id en el env
    [(id? fwael-expr) (interp (find-inenv (fwael-expr) env))]
    ;; Devuelve numV
    [(num? fwael-expr) (numV (num-n fwael-expr))]
    ;; Devuelve boolV
    [(bool? fwael-expr) (boolV (bool-b fwael-expr))]
    ;; Devuelve la lista vacia lisV '()
    [(lempty? fwael-expr) (listV empty)]
    ;; Devuelve el resultado de la operacion
    [(op? fwael-expr) ((op-f fwael-expr) (interp (op-args fwael-expr) env))]
    ;; Devuelve el resultado de la operacion
    [(op-bool? fwael-expr) (interp ((op-bool-f fwael-expr) (interp (op-bool-larg fwael-expr)) (interp (op-bool-rarg fwael-expr))))]
    ;; Devuelve una listaV
    [(lcons? fwael-expr) (listV (interp (lcons-l fwael-expr) env) (interp (lcons-r fwael-expr) env))]
    ;; Devuelve una listaV
    [(lcar? fwael-expr) (listV (interp (lcar-lst fwael-expr) env))]
    ;; Devuelve una listaV
    [(lcdr? fwael-expr) (listV (interp (lcdr-lst fwael-expr) env))]
    ;; Quita azucar para poder interpretar otra cosa que no sea with
    [(with? fwael-expr) (interp (desugar fwael-expr) env)]
    ;; Quita azucar para poder interpretar otra cosa que no sea with*
    [(with*? fwael-expr) (interp (desugar fwael-expr) env)]
    ;; Devuelve un closure
    [(fun? fwael-expr) (closureV (fun-params fwael-expr) (fun-body fwael-expr))]
    ;; Aplica la funcion
    [(app? fwael-expr) (interp (subst fwael-expr) env)]))

;; Perimetro elipse
;; Basada en la siguiente funcion
;;(define (perEli eli)
;;  (* pi (- (* 3 (+ (elipse-a eli) (elipse-b eli))) (sqrt (* (+ (* 3 (elipse-a eli)) (elipse-b eli)) (+ (elipse-a eli) (* 3 (elipse-b eli))))))))
;; Funcion
;; (fun (a b) (* (num 3.1415) (- (* (num 3) (+ a b)) (expt (*(+ (* (num 3) a) b) (+ a (* 3 b))) (num .5)))))
;; Cambiar parametros en la siguientes partes (apuntadas con "|")----------------------------------------------------------------|--|
(interp (desugar (app (fun ((id 'a) (id 'b)) (* (num 3.1415) (- (* (num 3) (+ (id 'a) (id 'b))) (expt (*(+ (* (num 3) (id 'a)) (id 'b)) (+ (id 'a) (* (num) (id 'b)))) (num .5))))) (5 10))) mtSub)