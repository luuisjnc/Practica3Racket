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
           (if (member sub-id (map binding-id bindings))
               (with* (subst-bindings bindings sub-id value) cuerpo)
               (with* (map
                       (lambda (b) (binding (binding-id b) (subst (binding-value b) sub-id value)))
                       bindings)
                      (subst cuerpo sub-id value)))]))

;; subst-bindings : (ListOf Binding) Symbol FWAE -> (ListOf Binding)
;; Realiza la sustitución correspondiente a todas las bindings de la
;; lista recibida hasta encontrar una aparición que la sombree.
(define (subst-bindings bindings sub-id value)
  (cond
    [(empty? bindings) bindings]
    [(symbol=? sub-id (binding-id (first bindings)))
     (cons
      (binding (binding-id (first bindings)) (subst (binding-value (first bindings)) sub-id value))
      (rest bindings))]
    [else (cons
           (binding (binding-id (first bindings)) (subst (binding-value (first bindings)) sub-id value))
           (subst-bindings (rest bindings) sub-id value))]))

;; interp : FWAE -> (or/c number? boolean?)
;; Evalúa una expresión WAE+ con alcance estático y evaluación
;; glotona. Para with y with* debe utilizar subst.
(define (interp expr)
  (type-case FWAE expr
    [num (n) n]
    [bool (b) b]
    [id (i) (error (format "Variable libre: ~a" i))]
    [op (f args) (apply f (map interp args))]
    [with (bindings cuerpo) (subst-cuerpo bindings cuerpo)]
    [with* (bindings cuerpo) (subst-cuerpo (subst-b bindings) cuerpo)]))

;; subst-cuerpo : (ListOf Binding) FWAE -> (or/c number? boolean?)
;; Sustituye los bindings en la expresión FWAE y la interpreta
(define (subst-cuerpo bindings cuerpo)
  (if (empty? bindings)
      (interp cuerpo)
      (subst-cuerpo
       (rest bindings)
       (subst cuerpo (binding-id (first bindings)) (fwaeifica (interp (binding-value (first bindings))))))))

;; fwaeifica : (or/c number? boolean?) -> FWAE
;; Convierte un valor interpretado un una expresión
;; FWAE para poder seguirla utilizando.
(define (fwaeifica v) (if (boolean? v) (bool v) (num v)))

;; subst-b : (ListOf Binding) -> (ListOf Binding)
;; Regresa la lista con los bindings que tienen el mismo id aplicados.
(define (subst-b bindings)
  (cond
    [(empty? bindings) bindings]
    [(member (binding-id (first bindings)) (map binding-id (rest bindings)))
     (subst-b (aux-subst-b (valua (first bindings)) (rest bindings)))]
    [else
     (let ([vb (valua (first bindings))])
     (cons
      vb
      (subst-b (aux-subst-b vb (rest bindings)))))]))

;; valua : Binding -> Binding
;; Valua la expresión dentro de un binding para asegurar que sea glotona.
(define (valua b) (binding (binding-id b) (fwaeifica (interp (binding-value b)))))

;; aux-subst-b : Binding (ListOf Binding) -> (ListOf Binding)
;; Hace las substituciones necesarias dentro de la lista de bindings.
(define (aux-subst-b b bindings)
  (cond
    [(empty? bindings) bindings]
    [(symbol=? (binding-id b) (binding-id (first bindings)))
     (cons
      (binding
       (binding-id b)
       (subst (binding-value (first bindings)) (binding-id b) (binding-value b)))
      (rest bindings))]
    [else (cons
           (binding
            (binding-id (first bindings))
            (subst (binding-value (first bindings)) (binding-id b) (binding-value b)))
           (aux-subst-b b (rest bindings)))]))

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
