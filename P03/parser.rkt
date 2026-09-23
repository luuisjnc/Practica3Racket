#lang plai
(require (file "./grammars.rkt"))

; ============================================================
; Lenguajes de Programación 2027-1
; Práctica 3 - Lenguaje WAE+
; parser.rkt
;
; INTEGRANTES: 
; - Cortes Nava Jose Luis (322115437)
; - Martínez García Emilio (322086689)
;
; ============================================================

; parse : s-expression -> FWAE
; Recibe una s-expression y construye el ASA correspondiente.
; Si la expresión no pertenece al lenguaje WAE+, debe lanzar:
;   "Syntax Error: expresion mal formada en parse"
;
(define (parse sexp)
  (cond
    [(number? sexp)  (num sexp)]
    [(boolean? sexp) (bool sexp)]
    [(symbol? sexp)  (id sexp)]
    [(list? sexp)
     (cond
       [(empty? sexp) (error-sintaxis)]
       [(eq? (first sexp) 'with)  (parse-with sexp #f)]
       [(eq? (first sexp) 'with*) (parse-with sexp #t)]
       [(operador? (first sexp))   (parse-operacion sexp)]
       [else (error-sintaxis)])]
    [else (error-sintaxis)]))

; Lanza el error pedido.
(define (error-sintaxis)
  (error "Syntax Error: expresion mal formada en parse"))

; operador? : any -> boolean
(define (operador? s)
  (and (symbol? s)
       (if (member s '(+ - * / modulo expt add1 sub1 = < > <= >= not))
           #t
           #f)))

; procedimiento-operador : symbol -> procedure
(define (procedimiento-operador s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(modulo) modulo]
    [(expt) expt]
    [(add1) add1]
    [(sub1) sub1]
    [(=) =]
    [(<) <]
    [(>) >]
    [(<=) <=]
    [(>=) >=]
    [(not) not]
    [else (error-sintaxis)]))

; aridad-valida? : symbol number -> boolean
(define (aridad-valida? operador n)
  (cond
    [(member operador '(add1 sub1 not))
     (= n 1)]
    [(member operador '(modulo expt = < > <= >=))
     (= n 2)]
    [(member operador '(+ - * /))
     (>= n 2)]
    [else #f]))

; parse-operacion : list -> FWAE
(define (parse-operacion sexp)
  (let* ([operador (first sexp)]
         [argumentos (rest sexp)])
    (if (aridad-valida? operador (length argumentos))
        (op (procedimiento-operador operador)
            (map parse argumentos))
        (error-sintaxis))))

; binding-valido? : any -> boolean
(define (binding-valido? b)
  (and (list? b)
       (= (length b) 2)
       (symbol? (first b))))

; parse-binding : s-expression -> Binding
(define (parse-binding b)
  (if (binding-valido? b)
      (binding (first b)
               (parse (second b)))
      (error-sintaxis)))

; id-repetido? : (listof s-expression) -> boolean
(define (id-repetido? bindings)
  (simbolo-repetido? (map first bindings)))

; simbolo-repetido? : (listof symbol) -> boolean
(define (simbolo-repetido? ids)
  (cond
    [(empty? ids) #f]
    [(member (first ids) (rest ids)) #t]
    [else (simbolo-repetido? (rest ids))]))

; bindings-validos? : (listof s-expression) -> boolean
(define (bindings-validos? bindings)
  (cond
    [(empty? bindings) #t]
    [(binding-valido? (first bindings))
     (bindings-validos? (rest bindings))]
    [else #f]))

; parse-with : list boolean -> FWAE
; secuencial? indica si se está parseando with* (#t) o with (#f).
(define (parse-with sexp secuencial?)
  (if (and (= (length sexp) 3)
           (list? (second sexp))
           (not (empty? (second sexp)))
           (bindings-validos? (second sexp)))
      (let ([bindings (second sexp)]
            [cuerpo (third sexp)])
        (if (and (not secuencial?)
                 (id-repetido? bindings))
            (error-sintaxis)
            (if secuencial?
                (with* (map parse-binding bindings)
                       (parse cuerpo))
                (with (map parse-binding bindings)
                      (parse cuerpo)))))
      (error-sintaxis)))


; Pruebas propias del parser.
(module+ test
  ; Se revisan todos los operadores y sus aridades.
  (for-each
   (lambda (simbolo funcion)
     (test (parse (list simbolo 1)) (op funcion (list (num 1))))
     (test/exn (parse (list simbolo)) "Syntax Error: expresion mal formada en parse")
     (test/exn (parse (list simbolo 1 2)) "Syntax Error: expresion mal formada en parse"))
   '(add1 sub1 not) (list add1 sub1 not))
  (for-each
   (lambda (simbolo funcion)
     (test (parse (list simbolo 1 2)) (op funcion (list (num 1) (num 2))))
     (test/exn (parse (list simbolo 1)) "Syntax Error: expresion mal formada en parse")
     (test/exn (parse (list simbolo 1 2 3)) "Syntax Error: expresion mal formada en parse"))
   '(modulo expt = < > <= >=) (list modulo expt = < > <= >=))
  (for-each
   (lambda (simbolo funcion)
     (test (parse (list simbolo 1 2)) (op funcion (list (num 1) (num 2))))
     (test (parse (list simbolo 1 2 3)) (op funcion (list (num 1) (num 2) (num 3))))
     (test/exn (parse (list simbolo)) "Syntax Error: expresion mal formada en parse")
     (test/exn (parse (list simbolo 1)) "Syntax Error: expresion mal formada en parse"))
   '(+ - * /) (list + - * /))
  ; El mensaje completo debe coincidir, sin prefijo "parse:".
  (for-each
   (lambda (expr)
     (test (with-handlers ([exn:fail? exn-message]) (parse expr))
           "Syntax Error: expresion mal formada en parse"))
   (list '() "x" '(+ 1 . 2) '(desconocido 1 2)
         '(with) '(with () x) '(with* () x) '(with x x)
         '(with ((1 2)) x) '(with ((x)) x) '(with ((x 1 2)) x)
         '(with ((x 1) (x 2)) x) '(with ((x 1)) x 2)
         '(with* ((x 1) y) x) '(with ((x (+ 1))) x)))
  (test (parse '{with* {{x 2} {x {* x 4}}} {+ x 1}})
        (with* (list (binding 'x (num 2))
                     (binding 'x (op * (list (id 'x) (num 4)))))
               (op + (list (id 'x) (num 1)))))
  (test (parse 1/2) (num 1/2))
  (test (parse 1+2i) (num 1+2i)))
