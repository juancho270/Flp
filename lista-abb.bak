#lang eopl


;GRAMATICA DE EL ARBOL NARIO
;arbol-binario := (arbol vacio) empty
;               := (nodo) <numero><arbol binario><arbol binario>


;Definicion del tipo de dato arbol nario
(define vacio  'vacio)

(define hoja (lambda (numero)
               (if (number? numero) (list numero) (eopl:error "valor esperado tipo numerico, ingresado: " numero))))

(define nodo (lambda (numero nodo-izquierdo nodo-derecho)
               (cond
                 ((not (number? numero)) (eopl:error "valor esperado tipo numerico, ingresado: "numero))
                 ((not (list? lista-arboles-narios)) (eopl:error "valor esperado tipo lista" ))
                 (else
                  (list numero lista-arboles-narios)
                  
                 )
                 )
               )
  )
;Predicados de arbolnario
(define vacio? (lambda (x)
                 (if (eq? x 'vacio) #t #f)
                 )
  )
(define arbol-hoja? (lambda (x)
                 (if (and (number? (car x)) (null? (cdr x))) #t #f)
                 )
  )
(define arbol-nodo? (lambda (x)
                 (if (and (number? (car x)) (list? (cdr x))) #t #f)
                 )
  )
;extractores de arbolnario
(define arbol>numero (lambda (x)
                      (car x)
                      )
  )

(define extraer-lista-arboles (lambda (x)
                                (cadr x)
                                ))


