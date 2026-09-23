#lang plai
(require (file "./grammars.rkt"))

;; ============================================================
;; Lenguajes de Programación 2027-1
;; Práctica 3 - Lenguaje WAE+
;; parser.rkt
;; ============================================================

;; parse : s-expression -> FWAE
;; Recibe una s-expression y construye el ASA correspondiente.
;; Si la expresión no pertenece al lenguaje WAE+, debe lanzar:
;;   "Syntax Error: expresion mal formada en parse"
;;
;; Recuerda verificar:
;;   - la forma de los bindings;
;;   - la aridad de los operadores;
;;   - que en with no haya identificadores repetidos;
;;   - que en with* sí se permite sombrear identificadores.
(define (parse sexp)
  ;; TODO: implementar.
  ...)

;; Puedes agregar funciones auxiliares debajo de parse.
