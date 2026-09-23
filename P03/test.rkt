#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./interp.rkt"))

;; ============================================================
;; Práctica 3 - Test
;;
;; Estas pruebas NO cubren todos los casos de la práctica.
;; Debes agregar pruebas propias en parser.rkt e interp.rkt.
;; ============================================================

(print-only-errors #t)

;; ------------------------------------------------------------
;; 1. Parser: valores atómicos
;; ------------------------------------------------------------
(test (parse 42) (num 42))
(test (parse -7) (num -7))
(test (parse #t) (bool #t))
(test (parse #f) (bool #f))
(test (parse 'x) (id 'x))

;; ------------------------------------------------------------
;; 2. Parser: estructura de with y with*
;; ------------------------------------------------------------
(test
 (parse '{with {{x 4} {y 9}} {+ x y}})
 (with (list (binding 'x (num 4))
             (binding 'y (num 9)))
       (op + (list (id 'x) (id 'y)))))

(test
 (parse '{with* {{x 2} {x 8}} x})
 (with* (list (binding 'x (num 2))
              (binding 'x (num 8)))
        (id 'x)))

;; ------------------------------------------------------------
;; 3. Parser: expresiones mal formadas
;; ------------------------------------------------------------
(test/exn (parse '{add1 1 2})
          "Syntax Error: expresion mal formada en parse")
(test/exn (parse '{modulo 10})
          "Syntax Error: expresion mal formada en parse")
(test/exn (parse '{with {{x 1 2}} x})
          "Syntax Error: expresion mal formada en parse")
(test/exn (parse '{with {{x 1} {x 2}} x})
          "Syntax Error: expresion mal formada en parse")

;; ------------------------------------------------------------
;; 4. Sustitución: casos básicos
;; ------------------------------------------------------------
(test (subst (id 'x) 'x (num 10))
      (num 10))
(test (subst (id 'y) 'x (num 10))
      (id 'y))
(test (subst (num 7) 'x (num 10))
      (num 7))
(test (subst (bool #t) 'x (num 10))
      (bool #t))

;; En with, el identificador ligado sombrea en el cuerpo,
;; pero NO en los valores de los bindings.
(test
 (subst (parse '{with {{x {+ x 1}}} {+ x y}})
        'x
        (num 10))
 (parse '{with {{x {+ 10 1}}} {+ x y}}))

;; En with*, el binding para x sombrea a la x externa para
;; los bindings posteriores y para el cuerpo.
(test
 (subst (parse '{with* {{y {+ x 1}} {x 4} {z {+ x 2}}} {+ x z}})
        'x
        (num 10))
 (parse '{with* {{y {+ 10 1}} {x 4} {z {+ x 2}}} {+ x z}}))

;; ------------------------------------------------------------
;; 5. Intérprete: valores y operaciones
;; ------------------------------------------------------------
(test (interp (parse 1729)) 1729)
(test (interp (parse #f)) #f)
(test (interp (parse '{add1 20})) 21)
(test (interp (parse '{sub1 20})) 19)
(test (interp (parse '{modulo 29 6})) 5)
(test (interp (parse '{expt 2 5})) 32)
(test (interp (parse '{+ 1 2 3 4})) 10)
(test (interp (parse '{- 20 3 2})) 15)
(test (interp (parse '{* 2 3 4})) 24)
(test (interp (parse '{/ 24 3 2})) 4)
(test (interp (parse '{not #f})) #t)
(test (interp (parse '{>= {expt 2 5} 30})) #t)

;; ------------------------------------------------------------
;; 6. Intérprete: alcance de with y with*
;; ------------------------------------------------------------
(test
 (interp (parse '{with {{x 7} {y 4}}
                  {+ {* x 2} y}}))
 18)

;; Los bindings de with son simultáneos.
(test/exn
 (interp (parse '{with {{x 4} {y {+ x 1}}} y}))
 "Variable libre: x")

;; Los bindings de with* son secuenciales.
(test
 (interp (parse '{with* {{x 2} {y {+ x 3}}} {* x y}}))
 10)

;; Sombreado.
(test
 (interp (parse '{with* {{x 10} {x 3} {y x}} {+ x y}}))
 6)

;; Variable libre.
(test/exn (interp (parse 'fantasma))
          "Variable libre: fantasma")
