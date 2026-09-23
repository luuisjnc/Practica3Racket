#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; ============================================================
;; Lenguajes de Programación 2027-1
;; Práctica 3 - Lenguaje WAE+
;; interp.rkt
;; ============================================================

;; subst : FWAE symbol FWAE -> FWAE
;; Realiza la sustitución expr[sub-id := value].
;; Debe sustituir únicamente apariciones libres y respetar el
;; alcance y sombreado de identificadores en with y with*.
(define (subst expr sub-id value)
  ;; TODO: implementar.
  ...)

;; interp : FWAE -> (or/c number? boolean?)
;; Evalúa una expresión WAE+ con alcance estático y evaluación
;; glotona. Para with y with* debe utilizar subst.
(define (interp expr)
  ;; TODO: implementar.
  ...)

;; Puedes agregar funciones auxiliares debajo de las funciones
;; principales que las utilicen.
