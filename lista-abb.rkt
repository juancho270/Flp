#lang eopl
;Juan Carlos Viteri Jimenez - 1427543
;Edgar Mauricio Ceron Florez - 1427918

;GRAMATICA DE EL ARBOL BINARIO
;arbol-binario := (arbol vacio) empty
;               := (nodo) <numero><arbol binario><arbol binario>


;Definicion del tipo de dato arbol binario

;Definicion de arbol vacio
(define vacio  'vacio)

;Definicion de una hoja
;num -> hoja
(define hoja (lambda (numero)
               (if (number? numero) (list numero) (eopl:error "valor esperado tipo numerico, ingresado: " numero))))

;Definicion de nodo 
;num nodo nod .> lista 
(define nodo (lambda (numero nodo-izquierdo nodo-derecho)
               (cond
                 [(not (number? numero)) (eopl:error "valor esperado tipo numerico, ingresado: "numero)]
                 [(not (list? nodo-izquierdo)) (eopl:error "valor esperado tipo lista" )]
                 [(not (list? nodo-derecho)) (eopl:error "valor esperado tipo lista" )]
                 (else
                  (list numero  nodo-izquierdo nodo-derecho) 
                 )
                 )
               )
  )


;Predicados de arbol binario

;Para saber si un arbol es vacio
;lista->Booleano
(define vacio? (lambda (x)
                 (if (eq? x 'vacio) #t #f)
                 )
  )

;Para saber si un arbol es hoja
;lista->Booleano
(define arbol-hoja? (lambda (x)
                 (if (and (number? (car x)) (vacio? (car (cdr x))) (vacio? (car (cdr (cdr  x))))) #t #f)
                 )
  )

;Para saber si un arbol es nodo
;lista->Booleano
(define arbol-nodo? (lambda (x)
                 (if (and (number? (car x)) (list? (cadr x)) (list? (car (cdr (cdr  list))))) #t #f)
                 )
  )
;extractores de arbolnario

;Extrae el numero de un arbol
;lista->numero
(define arbol-numero (lambda (x)
                       (if (vacio? x) vacio
                      (car x))
                      )
  )

;Extrae el nodo izquierdo de un arbol
;Lista -> lista
(define extraer-nodo-izquierdo (lambda (x)
                                 (if (vacio? x) vacio
                                (cadr x))
                                ))

;Extrae el nodo derecho de un arbol
;Lista -> lista

(define extraer-nodo-derecho (lambda (x)
                               (if (vacio? x) vacio
                                 (car (cdr (cdr  x))))
                                ))

;Definiciones del arbol del ejemplo del taller
(define nodo-izquierdo (list 3 (list 1 vacio vacio) (list 6 (list 4 vacio vacio ) (list 7 vacio vacio))))
(define nodo-derecho (list 10 vacio (list 14 (list 13 vacio vacio) vacio )))

;Funcion que me ayuda a saber si un arbol es binario de busqueda o no
;Lista -> booleano
(define ordenado? (lambda (list)
                    (cond
                      [(vacio? list) #true]
                      [(arbol-hoja? list) #true]
                      [(and (vacio? (extraer-nodo-derecho list)) (vacio? (extraer-nodo-izquierdo list))) #true]
                      [(and (vacio? (extraer-nodo-izquierdo list)) (not(vacio? (extraer-nodo-derecho list)))) (aux (arbol-numero list) (extraer-nodo-derecho list)) ]
                      [(and (vacio? (extraer-nodo-derecho list)) (not(vacio? (extraer-nodo-izquierdo list)))) (aux2 (arbol-numero list) (extraer-nodo-izquierdo list)) ]
                      [(and
                        (< (arbol-numero list) (arbol-numero (extraer-nodo-derecho list)))
                        (> (arbol-numero list) (arbol-numero (extraer-nodo-izquierdo list)))) (and #true (ordenado? (extraer-nodo-derecho list)) (ordenado? (extraer-nodo-izquierdo list)))]
                      [else #false]
                      )))

;Funcion auxiliar que apoya a ordenado? y compara el numero de un arbol con el numero de su nodod derecho
;num nodo -> booleano
(define aux (lambda (num nodo)
              (cond
                [(and (arbol-hoja? nodo) (> (arbol-numero nodo) num)) #true ]
                [else (and (> (arbol-numero nodo) num) (ordenado? nodo))])))

;Funcion auxiliar que apoya a ordenado? y compara el numero de un arbol con el numero de su nodod izquierdo
;num nodo -> booleano
(define aux2 (lambda (num nodo)
              (cond
                [(and (arbol-hoja? nodo) (< (arbol-numero nodo) num)) #true ]
                [else (and (< (arbol-numero nodo) num) (ordenado? nodo))])))

;Constructor del arbol
(define hacer-arbol (lambda (numero nodo nodo2)
                      (if (ordenado? (list numero nodo nodo2)) (list numero nodo nodo2) (eopl:error "El arbol que intenta crear no es un arbol binario de busqueda" ))))

(define nodo1 (hacer-arbol 8 nodo-izquierdo nodo-derecho))

;Funcion que inserta un numero al arbol binario
;numer Lista -> lista
(define insertar (lambda (num arbol)
    (if (esta-en-arbol num arbol) arbol                
   (cond
      
      [(vacio? arbol) (hacer-arbol num vacio vacio)]
      ((< num (arbol-numero arbol))
         (hacer-arbol (arbol-numero arbol) 
                    (insertar num (extraer-nodo-izquierdo arbol)) 
                    (extraer-nodo-derecho arbol)))
      ((> num (arbol-numero arbol))
         (hacer-arbol (arbol-numero arbol)
                    (extraer-nodo-izquierdo arbol)
                    (insertar num (extraer-nodo-derecho arbol))))
                  ))))

;Funcion que me dice si un numero esta o no en el arbol
;num Lista -> booleano
(define (esta-en-arbol num arbol)
   (cond 
      ((vacio? arbol) #f)
      ((= num (arbol-numero arbol)) #t)
      ((< num (arbol-numero arbol))
         (esta-en-arbol num (extraer-nodo-izquierdo arbol)))
      (else
         (esta-en-arbol num (extraer-nodo-derecho arbol)))))