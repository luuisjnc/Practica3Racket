#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))


;; ============================================================
;; Lenguajes de Programación 2027-1
;; Práctica 3 - Lenguaje WAE+
;; parser.rkt
;;
;; INTEGRANTES: 
;; - Cortes Nava Jose Luis (322115437)
;; - Martínez García Emilio (322086689)
;;
;; ============================================================
;;
;; subst : FWAE symbol FWAE -> FWAE
;; Realiza la sustitución expr[sub-id := value].
;; Debe sustituir únicamente apariciones libres y respetar el
;; alcance y sombreado de identificadores en with y with*.
(define (subst expr sub-id value)
  (type-case FWAE expr
    [id (i) (if (symbol=? i sub-id) value expr)]
    [num (n) expr]
    [bool (b) expr]
    [op (f args)
        (op f (map (lambda (arg) (subst arg sub-id value)) args))]
    [with (bindings cuerpo)
          (with (map (lambda (b)
                       (binding (binding-id b)
                                (subst (binding-value b) sub-id value)))
                     bindings)
                (if (member sub-id (map binding-id bindings))
                    cuerpo
                    (subst cuerpo sub-id value)))]
    [with* (bindings cuerpo)
           ; Te toca esta parte Emilio jeje :D.
           (error 'subst "TODO: implementar with* (parte Emlio)")]))

;; interp : FWAE -> (or/c number? boolean?)
;; Evalúa una expresión WAE+ con alcance estático y evaluación
;; glotona. Para with y with* debe utilizar subst.
(define (interp expr)
  (error 'interp "TODO: implementar interp (parte Emilio)"))

; Pruebas propias de la parte implementada.
(module+ test
  (test (subst (parse '{+ x {* y x}}) 'x (num 3))
        (parse '{+ 3 {* y 3}}))
  (test (subst (parse '{not x}) 'x (bool #f))
        (parse '{not #f}))
  (test (subst (parse '{with {{x 1} {y {+ x 2}}} {+ x y}})
               'x (num 10))
        (parse '{with {{x 1} {y {+ 10 2}}} {+ x y}}))
  (test (subst (parse '{with {{y x}} {+ x y}}) 'x (num 10))
        (parse '{with {{y 10}} {+ 10 y}}))
  (test (subst (parse '{with {{y x}} {with {{x x}} {+ x y}}})
               'x (num 10))
        (parse '{with {{y 10}} {with {{x 10}} {+ x y}}})))
