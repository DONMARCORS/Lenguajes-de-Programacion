#lang plai

;;Integrantes del Equipo
;; Marco Antonio Rivera Silva 318183583
;; Adrian Aguilera Moreno 421005200
;; Kevin Jair Torres Valencia 318331818

#| Ejercicio 1
Defina un tipo de datos abstracto Figura que sea utilizado para trabajar con figuras
geométricas, debe tener los siguientes constructores:
- Un constructor (triangulo a b c) donde a, b y c son números reales que
representan la longitud de los lados de un triángulo.
-Un constructor (rectangulo a b) donde a y b son números reales que representan
la altura y base de un rectangulo.
-Un constructor (rombo l D d) donde l, D y d son números reales que representan
el lado, diagonal mayor y diagonal menor de un rombo, respectivamente.
-Un constructor (paralelogramo a b h) donde a, b y h son números reales; donde
a y b representan los lados de un paralelogramo y h su altura.
-Un constructor (elipse a b) donde a y b son números reales y representan el
semieje mayor y el semieje menor de la elipse, respectivamente.

Definimos el tipo de dato Figura. |#

(define-type Figura
  [triangulo (a number?) (b number?) (c number?)]
  [rectangulo (a number?) (b number?)]
  [rombo (l number?) (D number?) (d number?)]
  [paralelogramo (a number?) (b number?) (h number?)]
  [elipse (a number?) (b number?)])


#| Ejercicio 2
Utilizando el tipo de datos Figura, defina las siguientes funciones:

perimetro: Figura --> number
Funcion que obtiene el perimetro de cada una de las figuras definidas previamente. |#

(define (perimetro figura)
  (cond
    [(triangulo? figura) (perTr figura) ]
    [(rectangulo? figura) (perRec figura)]
    [(rombo? figura) (perRom figura)]
    [(paralelogramo? figura) (perPar figura)]
    [(elipse? figura) (perEli figura)]))

;; Funcion auxiliar de perimetro que sirve para calcular el perimetro del Triangulo
(define (perTr triangulo)
  (+ (triangulo-a triangulo) (triangulo-b triangulo) (triangulo-c triangulo)))

;; Funcion auxiliar de perimetro que sirve para calcular el perimetro del Rectangulo
(define (perRec rectangulo)
  (+ (* 2 (rectangulo-a rectangulo)) (* 2 (rectangulo-b rectangulo))))

;; Funcion auxiliar de perimetro que sirve para calcular el perimetro del Rombo
(define (perRom rombo)
  (* 4 (rombo-l rombo)))

;; Funcion auxiliar de perimetro que sirve para calcular el perimetro del Paralelogramo
(define (perPar para)
  (+ (* 2 (paralelogramo-a para)) (* 2 (paralelogramo-b para))))

;; Funcion auxiliar de perimetro que sirve para calcular el perimetro de la Elipse
(define (perEli eli)
  (* pi (- (* 3 (+ (elipse-a eli) (elipse-b eli))) (sqrt (* (+ (* 3 (elipse-a eli)) (elipse-b eli)) (+ (elipse-a eli) (* 3 (elipse-b eli))))))))

#| La función (area f) que calcula el área de una Figura dada.
  area: Figura --> number
|#

;; Funcion que obtiene el area de cada una de las figuras definidas previamente.
(define (area figura)
  (cond
    [(triangulo? figura) (arTr figura) ]
    [(rectangulo? figura) (arRec figura)]
    [(rombo? figura) (arRom figura)]
    [(paralelogramo? figura) (arPar figura)]
    [(elipse? figura) (arEli figura)]))

;; Funcion auxiliar de area que sirve para calcular el area del Triangulo
(define (arTr tri)
  (let*([s (/ (+ (triangulo-a tri) (triangulo-b tri) (triangulo-c tri)) 2)])
    (sqrt (* s (- s (triangulo-a tri)) (- s (triangulo-b tri)) (- s (triangulo-c tri))))))

;; Funcion auxiliar de area que sirve para calcular el area del Rectangulo
(define (arRec rec)
  (* (rectangulo-a rec) (rectangulo-b rec)))

;; Funcion auxiliar de area que sirve para calcular el area del Rombo
(define (arRom rom )
  (/ (* (rombo-D rom) (rombo-d rom)) 2))

;; Funcion auxiliar de area que sirve para calcular el area del Paralelogramo
(define (arPar para)
  (* (paralelogramo-b para) (paralelogramo-h para)))

;; Funcion auxiliar de area que sirve para calcular el area de la elipse
(define (arEli eli)
  (* pi (elipse-a eli) (elipse-b eli)))

#| Ejercicio 3
Considere una clase de tren de pasajeros conformado por los siguientes tipos de
vagones:
-Locomotora. Posee una potencia de arrastre máxima, que consideraremos un entero
que indica el número de vagones no-locomotora que puede mover.
-Vagón de pasajeros. Posee una capacidad máxima de pasajeros que pueden abordarlo.
-Vagón restaurante. Posee un número de mesas y personal de servicio máximo.
-Vagón dormitorio. Posee un número fijo de camas.

Función que define el tipo de datos Vagon, que debe incluir 4 constructores;
uno por cada tipo de vagón descrito anteriormente:|#

(define-type Vagon
  [locomotora (p positive-integer?)]
  [pasajeros (cap positive-integer?)]
  [restaurante (mesas positive-integer?) (personal positive-integer?)]
  [dormitorio (camas positive-integer?)])

#| Definimos un predicado para permitir una creacion de trenes similar a la de
los ejemplos (Perdon por el nombre xd) |#
(define (alv? a)
  (if (or (Tren? a) (Vagon? a))
      #t
      #f))

;; Definimos el tipo de dato Tren
(define-type Tren
  [tren-loc (a locomotora?)]
  [tren-f (a Tren?) (b alv?)])


#| Ejercicio 4
Utilizando el tipo de datos Tren, defina las siguientes funciones:

-La función (num-pasajeros tren) que calcula el número de pasajeros máximo que
pueden abordar el tren. |#

(define (num-pasajeros tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (pasajeros? y)
                (+ (num-pasajeros x) (pasajeros-cap y))
                (+ (num-pasajeros x) 0))]))

#|-La función (arrastre-usado tren) que calcula el porcentaje de la potencia de
arrastre utilizada del tren. Debe regresar valores mayores a 100 si la capacidad
de arrastre de las locomotoras no es suficiente para mover el resto de los vagones
(y este resultado debe ser la proporción de arrastre que excede la capacidad
de las locomotoras).
arrastre-usado: Tren --> number
|#

(define (arrastre-usado tren)
  (cond
    [(eq? (contar-vagones tren) 0) (* (contar-locomotoras tren) 100)]
    [else (* (/ (contar-vagones tren) (contar-locomotoras tren)) 100)]))

;; Funcion auxiliar de arrastre-tren que cuenta la potencia de la locomotoras del tren
(define (contar-locomotoras tren)
  (type-case Tren tren
    [tren-loc (x) (locomotora-p (tren-loc-a tren))]
    [tren-f (x y)
            (if (tren-loc? y)
                (+ (contar-locomotoras x) (locomotora-p (tren-loc-a y)))
                (+ (contar-locomotoras x) 0))]))

;; Funcion auxiliar de arrastre-tren que cuenta la cantidad de vagones del tren
(define (contar-vagones tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (Vagon? y)
                (+ (contar-vagones x) 1)
                (+ (contar-vagones x) 0))]))

#|La función (sin-cama tren) que calcula el número de pasajeros que quedarían
sin cama durante; de acuerdo al total de pasajeros y camas en el tren. Considere
que las camas son individuales.
sin-cama: Tren --> nonnegative-integer
|#

(define (sin-cama tren)
  (if (>= (contar-camas tren) (contar-pasajeros tren))
      (- (contar-camas tren) (contar-pasajeros tren))
      (- (contar-pasajeros tren) (contar-camas tren))))

;; Funcion auxiliar de sin-cama que cuenta las camas del tren
(define (contar-camas tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (dormitorio? y)
                (+ (contar-camas x) (dormitorio-camas y))
                (+ (contar-camas x) 0))]))

;; Funcion auxiliar de sin-cama que cuenta el numero de pasajeros del tren
(define (contar-pasajeros tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (pasajeros? y)
                (+ (contar-pasajeros x) (pasajeros-cap y))
                (+ (contar-pasajeros x) 0))]))

#|-La función (max-comensales tren) que determina el número máximo de pasajeros
que pueden ser atendidos al mismo tiempo en los vagones restaurante del tren.
Considere que una mesa puede albergar 4 pasajeros y un personal de servicio puede
atender a 8 pasajeros. Tome en cuenta que la capacidad de las mesas y del personal
se limitan entre sí: la capacidad del personal puede exceder la capacidad de las
mesas, pero no es posible servir a un pasajero sin mesa; como puede haber mesas
suficientes para todos los pasajeros pero puede que no se cuente con personal
suficiente para atenderlos a todos.
max-comensales: Tren --> nonnegative-integer
|#
(define (max-comensales tren)
  (if (>= (* 8 (contar-personal tren)) (* 4 (contar-mesas tren)))
      (* 4 (contar-mesas tren))
      (* 8 (contar-personal tren))))

;; Funcion auxiliar de max-comensales que cuenta el personal del tren
(define (contar-personal tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (restaurante? y)
                (+ (contar-personal x) (restaurante-personal y))
                (+ (contar-personal x) 0))]))


;; Funcion auxiliar de max-comensales que cuenta las mesas del tren
(define (contar-mesas tren)
  (type-case Tren tren
    [tren-loc (x) 0]
    [tren-f (x y)
            (if (restaurante? y)
                (+ (contar-mesas x) (restaurante-mesas y))
                (+ (contar-mesas x) 0))]))

;; =====================================  Pruebas Unitarias  ======================================

;; ==================  Ejercicio 2  =======================

;;  perimetro Triangulo
(define (prueba-perimetro-Triangulo)
  (test (perimetro (triangulo 3 4 5)) 12))

;;  area Triangulo
(define (prueba-area-Triangulo)
  (test (area (triangulo 3 4 5)) 6))

;;  perimetro Rectangulo
(define (prueba-perimetro-Rectangulo)
  (test (perimetro (rectangulo 5 6)) 22))

;;  area Rectangulo
(define (prueba-area-Rectangulo)
  (test (area (rectangulo 5 6)) 30))

;;  perimetro Rombo
(define (prueba-perimetro-Rombo)
  (test (perimetro (rombo 3 6 8)) 12))

;;  area Rombo
(define (prueba-area-Rombo)
  (test (area (rombo 3 6 8)) 24))

;;  perimetro Paralelogramo
(define (prueba-perimetro-Paralelogramo)
  (test (perimetro (paralelogramo 3 6 8)) 18))

;;  area Paralelogramo
(define (prueba-area-Paralelogramo)
  (test (area (paralelogramo 3 6 8)) 48))

;;  perimetro Elipse
(define (prueba-perimetro-Elipse)
  (test (perimetro (elipse 3 3)) 18.84955592153876))

;;  area Elipse
(define (prueba-area-Elipse)
  (test (area (elipse 3 3)) 28.274333882308138))

;; ==================  Ejercicio 4  =======================
;; Número de pasajeros máximo
(define (prueba-num-pasajeros)
  (test (num-pasajeros (tren-f (tren-f (tren-loc (locomotora 4)) (pasajeros 15)) (pasajeros 16))) 31))

;; El porcentaje de la potencia de arrastre del tren
(define (prueba-arrastre-usado)
  (test (arrastre-usado (tren-f (tren-f (tren-f (tren-loc (locomotora 1)) (pasajeros 10)) (restaurante 5 2)) (dormitorio 10))) 300))

;; Número de pasajeros que quedarían sin cama
(define (prueba-sin-cama)
  (test (sin-cama (tren-f (tren-f (tren-loc (locomotora 1)) (dormitorio 20)) (pasajeros 40))) 20))

;; Número máximo de pasajeros que pueden ser atendidos al mismo tiempo en los vagones restaurante del tren.
(define (prueba-max-comensales)
  (test (max-comensales (tren-f (tren-f (tren-loc (locomotora 1)) (restaurante 7 3)) (pasajeros 10))) 24))
