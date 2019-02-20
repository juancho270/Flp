#lang eopl

;Juan Carlos Viteri Jimenez - 1427543
;Edgar Mauricio Ceron Florez - 1427918

;GRAMATICA DE EL ARBOL BINARIO
;arbol-binario := (arbol vacio) empty
;               := (nodo) <numero><arbol binario><arbol binario>


;Definicion del tipo de dato arbol binario

;Definicion de arbol
(define-datatype arbol-binario arbol-binario?
  (arbol-vacio)
  (nodo (dato number?)
        (hijoizquierdo arbol-binario?)
        (hijoderecho arbol-binario?)))

;Para saber si un arbol es hoja
;arbol->Booleano
(define arbol-hoja?
  (lambda (arbol)
    (cases arbol-binario arbol
      (arbol-vacio() #f)
      (nodo (field hoja1 hoja2)
            (cond
              [(and (arbol-vacio? hoja1) (arbol-vacio? hoja2)) #t]
              [else #f])) 
      )
    )
  )

;Para saber si un arbol es nodo
;arbol->Booleano
(define arbol-nodo?
  (lambda (arbol)
    (cases arbol-binario arbol
      (arbol-vacio() #f)
      (nodo (field hoja1 hoja2)
            (cond
              [(and (arbol-vacio? hoja1) (arbol-vacio? hoja2)) #f]
              [else #t])) 
      )
    )
  )

;Para saber si un arbol es vacio
;arbol->Booleano

(define arbol-vacio?
  (lambda (arbol)
    (cases arbol-binario arbol
      (arbol-vacio () #t)
      (nodo (field hoja1 hoja2) #f))))

;extractores de arbolnario

;Extrae el numero de un arbol
;arbol->numero
(define arbol-numero
  (lambda (arbol)
    (cases arbol-binario arbol
      (arbol-vacio() (arbol-vacio))
      (nodo (field hoja1 hoja2)
            field) 
      )
    )
  )

;Extrae el nodo izquierdo de un arbol
;arbol->arbol
(define extraer-nodo-izquierdo
  (lambda (arbol)
    (cases arbol-binario arbol
      (arbol-vacio() (arbol-vacio))
      (nodo (field hoja1 hoja2)
            hoja1) 
      )
    )
  )

;Extrae el nodo derecho de un arbol
;arbol -> arbol
(define extraer-nodo-derecho
  (lambda (arbol)
    (cases arbol-binario arbol
      (arbol-vacio() (arbol-vacio))
      (nodo (field hoja1 hoja2)
            hoja2) 
      )
    )
  )

;Funcion que me ayuda a saber si un arbol es binario de busqueda o no
;arbol -> booleano
(define ordenado?
  (lambda (arbol)
    (cases arbol-binario arbol
      (arbol-vacio() #t)
      (nodo (field hoja1 hoja2)
            (cond
               [(arbol-vacio? arbol) #true]
                      [(arbol-hoja? arbol) #true]
                [(and (arbol-vacio? (extraer-nodo-izquierdo arbol)) (not(arbol-vacio? (extraer-nodo-derecho arbol)))) (aux (arbol-numero arbol) (extraer-nodo-derecho arbol)) ]
                      [(and (arbol-vacio? (extraer-nodo-derecho arbol)) (not(arbol-vacio? (extraer-nodo-izquierdo arbol)))) (aux2 (arbol-numero arbol) (extraer-nodo-izquierdo arbol)) ]
                      [(and
                        (< (arbol-numero arbol) (arbol-numero (extraer-nodo-derecho arbol)))
                        (> (arbol-numero arbol) (arbol-numero (extraer-nodo-izquierdo arbol)))) (and #true (ordenado? (extraer-nodo-derecho arbol)) (ordenado? (extraer-nodo-izquierdo arbol)))]
                      [else #false])) 
      )
    )
  )

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



;Funcion que inserta un numero al arbol binario
;numer Lista -> lista
(define insertar
  (lambda (num arbol)
    (cases arbol-binario arbol
      (arbol-vacio() (hacer-arbol num (arbol-vacio) (arbol-vacio)))
      (nodo (field hoja1 hoja2)
            (if (esta-en-arbol num arbol) arbol                
   (cond
      
      [(arbol-vacio? arbol) (hacer-arbol num (arbol-vacio) (arbol-vacio))]
      ((< num (arbol-numero arbol))
         (hacer-arbol (arbol-numero arbol) 
                    (insertar num (extraer-nodo-izquierdo arbol)) 
                    (extraer-nodo-derecho arbol)))
      ((> num (arbol-numero arbol))
         (hacer-arbol (arbol-numero arbol)
                    (extraer-nodo-izquierdo arbol)
                    (insertar num (extraer-nodo-derecho arbol))))
                  ))) 
      )
    )
  )

;Funcion que me dice si un numero esta o no en el arbol
;num Lista -> booleano
(define esta-en-arbol
  (lambda (num arbol)
    (cases arbol-binario arbol
      (arbol-vacio() #f)
      (nodo (field hoja1 hoja2)
            (cond 
      ((arbol-vacio? arbol) #f)
      ((= num (arbol-numero arbol)) #t)
      ((< num (arbol-numero arbol))
         (esta-en-arbol num (extraer-nodo-izquierdo arbol)))
      (else
         (esta-en-arbol num (extraer-nodo-derecho arbol))))) 
      )
    )
  )

;recorrido preorden
(define preorden
  (lambda ( arbol)
    (cases arbol-binario arbol
      (arbol-vacio() empty)
      (nodo (field hoja1 hoja2)
           (append (cons field (preorden hoja1)) (preorden hoja2))))
                         
      )
    )
;recorrido inorden
(define inorden
  (lambda ( arbol)
    (cases arbol-binario arbol
      (arbol-vacio() empty)
      (nodo (field hoja1 hoja2)
           (append (inorden hoja1)
            (cons field empty)
            (inorden hoja2))
           ))
                         
      )
    )
;recorrido postorden
(define postorden
  (lambda ( arbol)
    (cases arbol-binario arbol
      (arbol-vacio() empty)
      (nodo (field hoja1 hoja2)
             (append  (postorden hoja1)
           (postorden hoja2)
             (cons field empty)) )
          )
                         
      )
    )
 

;Pruebas ;nodo1 es el arbol del taller
(define nodo-izquierdo  (nodo 3 (nodo 1 (arbol-vacio) (arbol-vacio))    (nodo 6 (nodo 4 (arbol-vacio) (arbol-vacio)) (nodo 7 (arbol-vacio) (arbol-vacio)))))
(define nodo-derecho  (nodo 10  (arbol-vacio)  (nodo 14 (nodo 13 (arbol-vacio) (arbol-vacio)) (arbol-vacio) )))
(define nodo1 (hacer-arbol 8 nodo-izquierdo nodo-derecho))
(postorden nodo1)
(preorden nodo1)
(inorden nodo1)
(esta-en-arbol 5 nodo1)
(insertar 5 nodo1)
(insertar 3 nodo1)
(ordenado? nodo1)
(extraer-nodo-derecho nodo1)
(extraer-nodo-izquierdo nodo1)
(arbol-numero nodo1)
(arbol-hoja? nodo1)

