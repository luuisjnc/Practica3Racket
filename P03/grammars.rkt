#lang plai

;; ============================================================
;; Lenguajes de Programación 2027-1
;; Práctica 3 - Lenguaje WAE+
;; grammars.rkt
;;
;; IMPORTANTE: No modificar este archivo.
;; ============================================================

;; Binding representa una asignación local: identificador y valor.
(define-type Binding
  [binding (id symbol?)
           (value FWAE?)])

;; Árbol de Sintaxis Abstracta del lenguaje WAE+.
(define-type FWAE
  [id    (i symbol?)]
  [num   (n number?)]
  [bool  (b boolean?)]
  [op    (f procedure?)
         (args (listof FWAE?))]
  [with  (bindings (listof binding?))
         (body FWAE?)]
  [with* (bindings (listof binding?))
         (body FWAE?)])
