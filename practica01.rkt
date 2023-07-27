#lang plai
;;Integrantes del Equipo
;; Marco Antonio Rivera Silva 318183583
;; Adrian Aguilera Moreno 421005200
;; Kevin Jair Torres Valencia 318331818

#| Ejercicio 1
Defina una función que filtre el contenido de una lista dado un predicado.
Un predicado es una función que devuelve un valor booleano (#t ó #f). |#
;; filtra-lista: (listof any) procedure -> (listof any)

(define (filtra-lista lista predicado)
  (if(empty? lista)
     '()
     (if(predicado (first lista))
        (append (list (first lista)) (filtra-lista (rest lista) predicado))
        (filtra-lista (rest lista) predicado))))

#| Ejercicio 2
Defina una función que tome una lista y devuelva otra del mismo tamaño,
cuyos elementos describen en texto el tipo de datos de los elementos en la
lista original.|#
;; tipos-lista: (listof any) → (listof string)

(define (tipos-lista lista)
    (if (empty? lista)
        empty
        (let* ([elem (first lista)]
                [res (cond
                      [(boolean? elem) "boolean"]
                      [(number? elem) "number"]
                      [(char? elem) "char"]
                      [(string? elem) "string"]
                      [(symbol? elem) "symbol"]
                      [(list? elem) "list"]
                      [(pair? elem) "pair"]
                      [else "Otra cosa xd"])])
            (append (list res) (tipos-lista (rest lista))))))

#| Ejercicio 3
Se dice que un número natural es raro si al sumar cada una de sus cifras
elevadas al número de cifras que lo forman, se obtiene el número original.
Por ejemplo, el número 153 (que tiene 3 cifras) es raro, pues
153 = 1^3+ 5^3 + 3^3|#
;; raro?: number → boolean

(define (raro? num)
  (if (= (sumaDigitos num (length (string->list(number->string num)))) num)
      #t
      #f))

;; Funcion Auxiliar del ejercicio 3 que suma todos los digitos elevado a la potencia indicada
(define (sumaDigitos x long) 
  (if (= x 0)
      0
      (+ (expt (modulo x 10) long) (sumaDigitos (/ (- x (modulo x 10)) 10) long))))

#| Ejercicio 4
Defina un predicado que dado un número arbitrario de números, indique si están
ordenados en forma descendente o no.|#
;; descendente?: number* → boolean

(define descendente? (lambda nums
    (apply >= nums)))

#| Ejercicio 5
Defina un predicado que tome una cadena e indique si esta es un palíndromo.
Un palíndromo es una cadena que se escribe igual que invirtiendo el orden de
sus caracteres.|#
;; palindromo?: string -> boolean

(define (palindromo? cadena)
  (if (equal? (string->list cadena) (reverse (string->list cadena)))
      #t
      #f))

#| Ejercicio 6
Defina un predicado que indica si un número entero es primo. Un número primo es
aquel que solo es divisible por 1 y sí mismo (divisible a su vez significa que
al dividir el número por otro se obtiene cero como residuo; la división es
exacta).|#
;; primo?: number → boolean

(define (primo? numero)
  (primoAux numero 2))

;; Funcion auxiliar del ejercicio 6 que checa todos los requisitos para ver si es un numero primo.
(define (primoAux num div)
  (cond
    [(and (<= num 2) (= num 2)) #t]
    [(and (<= num 2) (not (= num 2))) #f]
    [(= (modulo num div) 0) #f]
    [(> (* div div) num) #t]
    [else (primoAux num (+ div 1))]))

#| Ejercicio 7
Defina una función que indique todas las posibles formas de obtener una cantidad
entera positiva a partir de monedas de $1, $2 y $5 sin repeticiones.

Se considera una combinación repetida aquella en la que únicamente cambian de
lugar las monedas. Por ejemplo, para obtener 4; podríamos tener las
combinaciones ($1, $2, $1) y ($2, $1, $1); pero en ambos casos tenemos una
moneda de $2 y dos de $1; por lo que solo están cambiando de lugar y las
consideraremos repetidas.|#
;; num-comb-monedas: number → number
(define (num-comb-monedas cantidad)
  (let*-values(
              [(cuantos10 falta) (quotient/remainder cantidad 10)]
              [(casos10) (* cuantos10 10)])
              (cond
                [(= falta 0) casos10]
                [(= falta 1) (+ casos10 1)]
                [(= falta 2) (+ casos10 2)]
                [(= falta 3) (+ casos10 2)]
                [(= falta 4) (+ casos10 3)]
                [else (- cantidad 1)])))


#| Ejercicio 8
Define una función que tome una lista de números y devuelva el promedio, moda y
mediana de los elementos en la lista.

El promedio es la suma de todos los elementos dividida por el total de elementos
en la lista.
La moda es el conjunto de valores que más se repite en la lista.
Por último, la mediana es el valor en la posición central de la lista; es decir,
en listas de tamaño impar, es el elemento cuya posición corresponde la mitad
del tamaño de la lista y en listas de tamaño par, es el promedio de los
valores cuya posición corresponde con el piso y techo de la mitad del tamaño de
la lista.|#
;; prom-mod-med: (listof number) → number (listof number) number

(define (prom-mod-med lista)
  (if (empty? lista)
      (values "Sin datos" (list "Sin datos") "Sin datos")
      (values (pro lista) (mode lista) (med lista))))

;; Funcion Auxiliar del ejercicio 8 que obtiene el promedio
(define (pro lista)
  (/ (apply + lista) (length lista)))

;; Funcion Auxiliar del ejercicio 8 que obtiene la moda
(define (mode lista)
  '("Hola"))

;; Funcion Auxiliar del ejercicio 8 que obtiene la mediana
(define (med lista)
  (if(= (modulo (length lista) 2) 0)
     (med1centro lista)
     (med2centro lista)))

;; Funcion Auxiliar del ejercicio 8 que obtiene la mediana cuando el numero de elementos es par
(define (med1centro lista)
  (let ([lista-ord (sort lista <)])
    (/(+ (list-ref lista-ord (- (/ (length lista-ord) 2) 1)) (list-ref lista-ord (/ (length lista-ord) 2))) 2)))

;; Funcion Auxiliar del ejercicio 8 que obtiene la mediana cuando el numero de elementos es impar
(define (med2centro lista)
  (let ([lista-ord (sort lista <)])
    (list-ref lista-ord (floor(/ (length lista-ord) 2)))))

#| Ejercicio 9
Defina una función que genere en una lista, todas las posibles rotaciones de
elementos de una lista dada.|#
;; rota: (listof any) -> (listof (listof any))

(define (rota lista)
  (if (empty? lista)
      empty
      (rotaAux lista (length lista))))

;; Fiuncion auxiliar del ejercicio 9 que rota la lista una cantidad de veces igual a la longitud de la lista
(define (rotaAux lista longitud)
  (if (zero? longitud)
      empty
      (cons lista (rotaAux (rotaIzq lista) (- longitud 1)))))

;; Funcion auxiliar del ejercicio 9 que rota la lista a la izquierda
(define (rotaIzq lista)
  (append (rest lista) (list (first lista))))

#| Ejercicio 10
Defina una función que tome una lista de enteros y determine si representa el
inicio de una secesión geométrica.
Los elementos en una sucesión geométrica tienen la forma a_i = a_1 ∗ r^(i−1).
Si en efecto la lista dada representa una sucesión cuyos elementos satisfacen
esta regla, devuelve una lista de cadenas del mismo tamaño que la original en
la que cada entrada representa los elementos de la lista original en su forma
a_i = a_1 ∗ r^(i−1). En otro caso, devuelve la lista vacía.

Considere que la razón de una sucesión geométrica se obtiene como
r = (a_n)/(a_n−1) y que a_1 es el primer elemento en la lista dada.|#
;; extender-suc-geom: (listof number) → (listof string)

(define (extender-suc-geom lista)
  (let* ([n (length lista)]
         [i 1]
         [a1 (car lista)]
         [r (/ (list-ref lista (- (length lista) 1)) (list-ref lista (- (length lista) 2)))]
         [lista2 (aux-regresa-lista n r a1 i)])
    (if (and (aux-compara lista lista2) #t)
        (aux-geom n i a1 r)
        '())))

;; Funcion auxiliar del ejercicio 10 que hace que se imprima como en los ejemplos
(define (aux-geom n i a1 r)
  (define (formato n i al r)
    (string-append (number->string a1) "*" (number->string r) "^(" (number->string i) "-1)"))
  (if(= i n)
     (cons (formato n i a1 r) '())
     (cons (formato n i a1 r) (aux-geom n (+ 1 i) a1 r))))

;; Funcion auxiliar del ejercicio 10 para calcular a1 * r^(i - 1)
(define (auxCalcular r a1 i)
  (* a1 (expt r (sub1 i))))

;; Funcion auxiliar del ejercicio 10 para regresar la lista con los elementos
(define (aux-regresa-lista n r a1 i)
  (if (= n i)
      (cons (auxCalcular r a1 i) '())
      (cons (auxCalcular r a1 i) (aux-regresa-lista n r a1 (+ 1 i)))))

;; Funcion auxiliar del ejercicio 10 para extender-suc-geom que compara 2 listas
(define (aux-compara lista1 lista2)
  (if (empty? lista1)
      #t
      (if (= (first lista1) (first lista2))
          (and #t (aux-compara (rest lista1) (rest lista2)))
          (and #f))))
#| Ejercicio 11
Defina funciones que realicen una prueba unitaria de cada uno de los ejercicios
en la práctica; es decir para obtener el punto extra deberá definir 10 funciones
de prueba. Utilice la función incluida en el lenguaje plai test para realizarlo.|#

;; =====================================  Pruebas Unitarias  ======================================

;; Ejercicio 1
(define (prueba1-filtra-lista)
  (test (filtra-lista (list 1 "asdas" 2 2 2) number?) (list 1 2 2 2)))

(define (prueba2-filtra-lista)
  (test (filtra-lista (list 1 "asdas" 2 (list 1 2 3) 4) list?) (list (list 1 2 3))))

;; Ejercicio 2
(define (prueba1-tipos-lista)
  (test (tipos-lista (list 1 "hola" 2 (list 1 2) 3 'a 'b)) (list "number" "string" "number" "list" "number" "symbol" "symbol")))

(define (prueba2-tipos-lista)
  (test (tipos-lista (list 1 2 "hola" #\a 3 4 empty "adios")) (list "number" "number" "string" "char" "number" "number" "list" "string")))

;; Ejercicio 3
(define (prueba1-raro?)
  (test (raro? 12) #f))

(define (prueba2-raro?)
  (test (raro? 153) #t))

;; Ejercicio 4
(define (prueba1-descendente?)
  (test (descendente? 5 4 6 2 1) #f))

(define (prueba2-descendente?)
  (test (descendente? 5 4 3 2 1) #t))

;; Ejercicio 5
(define (prueba1-palindromo?)
  (test (palindromo? "Saken las kwamas") #f))

(define (prueba2-palindromo?)
  (test (palindromo? "girafarig") #t))

;; Ejercicio 6
(define (prueba1-primo?)
  (test (primo? 8) #f))

(define (prueba2-primo?)
  (test (primo? 11) #t))

;; Ejercicio 7
(define (prueba1-num-comb-monedas)
  (test (num-comb-monedas 7) 6))

(define (prueba2-num-comb-monedas)
  (test (num-comb-monedas 10) 10))

;; Ejercicio 8
;; Estas pruebas muestra los resultados uno por uno, porque no pudimos hacer que diera el resultado con los 3 datos a la vez.
;; Solo hicimos promedio y mediana.
(define (prueba1-prom-mod-med)
  (test (pro (list 10 7 4 6 8 10 10 9)) 8))

(define (prueba2-prom-mod-med)
  (test (med (list 10 7 4 6 9 10 10 9)) 9))

(define (prueba3-prom-mod-med)
  (test (pro (list 2 8 7 3 5)) 5))

(define (prueba4-prom-mod-med)
  (test (med (list 2 8 7 3 5)) 5))

;; Ejercicio 9
(define (prueba1-rota)
  (test (rota (list 1 2 3)) (list (list 1 2 3) (list 2 3 1) (list 3 1 2))))

(define (prueba2-rota)
  (test (rota (list "hola" #f 5)) (list (list "hola" #f 5) (list #f 5 "hola") (list 5 "hola" #f))))

;; Ejercicio 10
(define (prueba1-extender-suc-geom)
  (test (extender-suc-geom (list 1 2 4 8 16 32))
        (list "1*2^(1-1)" "1*2^(2-1)" "1*2^(3-1)" "1*2^(4-1)" "1*2^(5-1)" "1*2^(6-1)")))

(define (prueba2-extender-suc-geom)
  (test (extender-suc-geom (list 5 10 20 40 80))
        (list "5*2^(1-1)" "5*2^(2-1)" "5*2^(3-1)" "5*2^(4-1)" "5*2^(5-1)")))