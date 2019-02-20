#lang racket

;Juan Carlos Viteri Jimenez - 1427543
;Edgar Mauricio Ceron Florez - 1427918


;PUNTO 1.1
;Funcion que retorna x veces y
;int lista -> lista
(define copy
  (lambda (x y)
    (if (number? x)
    (if (= x 0) empty
    (if (= x 1) (cons y empty) (cons y (copy (- x 1) y )))) "El primer argumento debe ser un numero")))
;---------------------------------------------------------------------------------------------------------------------------------------------------------
;Punto 1.2

;Funcion que recibe una funcion unaria y dos listas, retorna una lista de pares  en donde se evalua cada elemento de la primera lista
;con la función y si el resultado existe en la segunda lista devuelve una lista de pares con los elementos de ambas listas
;proc lista lista -> lista

(define mapping
  (lambda (func l1 l2)
    (if (procedure? func)
    (local(
            ;Aplica la funcion a un numero y compara el resultado con los elementos de la lista 2 y si son iguales retorna una tupla con el numero y el elemento
            ; de la lista
            ;proc numero lista -> lista ( (a, b) , (a, b) , ....)
            (define evaluador (lambda (func num l2) (cond
                                                      [(empty? l2) empty]
                                                      [(= (func num) (first l2)) (cons (list num (first l2)) (evaluador func num (rest l2)))]
                                                      [else (evaluador func num (rest l2))]
                                                      )))
            ;Funcion que recibe una funcion y 2 listas, pasa cada elemento de la l1 a evaluador para retornar una lista de tuplas
            ;proc lista lista -> lista
            (define cuerpo (lambda (func l1 l2) (cond
                                                  [(empty? l1) empty]
                                                  [else (append (evaluador func (first l1) l2) (cuerpo func (rest l1) l2))]))))
       (cuerpo func l1 l2)) "El primer elemento debe ser una funcion lambda")))
      

;----------------------------------------------------------------------------------------------------------------------
;Punto 1.3
;Funcion que crea un tupla de los elementos ingresados
;int -> lista
(define creaTupla
  (lambda y y))

;Funcion que retorna el producto cartesiano de 2 listas
;Lista, Lista -> Lista
(define product
(lambda (A B)
(append* (map (lambda (a) (map (lambda (b) (cons a(if (list? b) b (creaTupla b)))) B))A))))

;-------------------------------------------------------------------------------------------------------
;Punto 1.4
;Funcion que retorna el primer elemento de una lista que cumpla con el predicado
;predicado,lista -> booleano or elemento
(define list-index
  (lambda (predicado lista) (contar predicado lista 0)))

(define contar (lambda (predicado lista num)
    (cond  ((empty? lista) #F)
      ((predicado (first lista)) num )
          (else (contar predicado (rest lista) (+ num 1))))))
  
(define x (list 2 4 6))
(define y (list 4 8))
(define f (lambda(x) (* x 2)))


;Pruebas
(copy 5 'five)
(copy 3 (list 1 2 3 4) )
(copy 0 (list 1 2) )
(copy 'a (list 1 2) )
(copy 5 5)
(mapping (lambda (d) (* d 2)) (list 1 2 3 4) (list 2 4 6) )
(mapping (lambda (d) (* d 3)) (list 1 2 3 4) (list 2 4 6) )
(mapping (lambda (d) (* d 2)) (list 1 2 3 4) (list 3 9 12) )
(mapping 5 (list 1 2 3 4) (list 3 9 12) )
(mapping (lambda (d) (* d 4)) (list 2 4 6) (list 8 16 24 100 50 12 40 8))
(product '(1 2 3) '(4 5 6))
(product '(a b c) '(x y) )
(list-index number? '(a 2 (1 3) b 7) )
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3) )
