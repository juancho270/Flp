#lang eopl
;Juan Carlos Viteri Jimenez - 1427543
;Edgar Mauricio Ceron Florez - 1427918

;GRAMATICA DE EL ARBOL BINARIO
;arbol-binario := (arbol vacio) empty
;               := (nodo) <numero><arbol binario><arbol binario>


;Definicion del tipo de dato arbol nario

(define vacio (lambda () (lambda () 'vacio)))

;Definicion de una hoja
;num -> hoja

(define hoja (lambda (numero) (lambda ()
                                (if (number? numero)
                                    (list numero vacio vacio); si es un numero
                                    (eopl:error "valor esperado tipo numerico, ingresado: " numero)
                                    )
                                )
               )
  )


;Definicion de nodo 
;num nodo nod .> lista 
(define nodo (lambda (numero nododerecho nodoizquierdo) (lambda ()
                                                     (cond
                                                       [(not (number? numero)) (eopl:error "valor esperado tipo numerico, ingresado: " numero)]
                                                       [(not (procedure? nodoizquierdo)) (eopl:error "valor esperado tipo nodo")]
                                                       [(not (procedure? nododerecho))  (eopl:error "valor esperado tipo nodo")]
                                                        [else
                                                         (list numero nododerecho nodoizquierdo)
                                                         ]
                                                        )
                                                     )
               )
  )
                                                     
;Predicados de arbolnario

;Para saber si un arbol es vacio
;lista->Booleano
(define vacio? (lambda (x)
                 (if (procedure? x)
                 (if (eq? x vacio) #t #f)
                 (if (eq? x vacio) #t #f)
                 ))
  )

;Para saber si un arbol es hoja
;lista->Booleano
(define arbol-hoja? (lambda (x)
                 (if  (procedure?  x)
                     (if (and (number? (car (x))) (vacio? (car (cdr (x)))) (vacio? (car (cdr (cdr  (x)))))) #t #f)  #f
                 )
  )
  )

;Para saber si un arbol es nodo
;lista->Booleano
(define arbol-nodo? (lambda (x)
                 (if (procedure? x)
                    (and (number? (car (x))) (procedure?  (car (cdr (x)))) (procedure? (car (cdr (cdr (x)))))) 
                     #false
                 
                 )
  
  )
)

;extractores de arbolnario
(define arbol-numero (lambda (x)
                      (car( x))
                      )
  )

;Extrae el nodo izquierdo de un arbol
;Lista -> lista
(define extraer-nodo-izquierdo (lambda (x)
                                 (if (vacio? (x)) vacio
                                (cadr (x)))
                                ))

;Extrae el nodo derecho de un arbol
;Lista -> lista

(define extraer-nodo-derecho (lambda (x)
                               (if (vacio? (x)) vacio
                                 (car (cdr (cdr  (x)))))
                                ))

;Definiciones del arbol del ejemplo del taller
(define nodo-izquierdo  (nodo 3 (hoja 1) (nodo 6 (hoja 4) (hoja 7))))
(define nodo-derecho  (nodo 10  vacio  (nodo 14 (hoja 13) vacio )))

;Funcion que me ayuda a saber si un arbol es binario de busqueda o no
;Lista -> booleano

(define ordenado? (lambda (arbol)
                    (cond
                      [(vacio? arbol) #true]
                      [(arbol-hoja? arbol) #true]
                      [(and (vacio? (extraer-nodo-derecho arbol)) (vacio? (extraer-nodo-izquierdo arbol))) #true]
                      [(and (vacio? (extraer-nodo-izquierdo arbol)) (not(vacio? (extraer-nodo-derecho arbol)))) (aux (arbol-numero arbol) (extraer-nodo-derecho arbol)) ]
                      [(and (vacio? (extraer-nodo-derecho arbol)) (not(vacio? (extraer-nodo-izquierdo arbol)))) (aux2 (arbol-numero arbol) (extraer-nodo-izquierdo arbol)) ]
                      [(and
                        (< (arbol-numero arbol) (arbol-numero (extraer-nodo-derecho arbol)))
                        (> (arbol-numero arbol) (arbol-numero (extraer-nodo-izquierdo arbol)))) (and #true (ordenado? (extraer-nodo-derecho arbol)) (ordenado? (extraer-nodo-izquierdo arbol)))]
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
(define hacer-arbol (lambda (numero nodo1 nodo2)
                      (if (ordenado? (nodo numero nodo1 nodo2)) (nodo numero nodo1 nodo2) (eopl:error "El arbol que intenta crear no es un arbol binario de busqueda" ))))
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

;Pruebas ; nodo 1 es el arbol del taller
(esta-en-arbol 5 nodo1)
(insertar 5 nodo1)
(insertar 3 nodo1)
(ordenado? nodo1)
(extraer-nodo-derecho nodo1)
(extraer-nodo-izquierdo nodo1)
(arbol-numero nodo1)
(arbol-hoja? nodo1)