#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;LOGICA DEL JUEGO;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Función baraja
;Recibe un id diferente de cero y devuelve la baraja
(define(baraja id)
  (cond((zero? id)
        '())
       (else
        '((1 "Ace" "D")(1 "Ace" "S")(1 "Ace" "T")(1 "Ace" "H")
                       (2 "2" "D")(2 "2" "S")(2 "2" "T")(2 "2" "H")
                       (3 "3" "D")(3 "3" "S")(3 "3" "T")(3 "3" "H")
                       (4 "4" "D")(4 "4" "S")(4 "4" "T")(4 "4" "H")
                       (5 "5" "D")(5 "5" "S")(5 "5" "T")(5 "5" "H")
                       (6 "6" "D")(6 "6" "S")(6 "6" "T")(6 "6" "H")
                       (7 "7" "D")(7 "7" "S")(7 "7" "T")(7 "7" "H")
                       (8 "8" "D")(8 "8" "S")(8 "8" "T")(8 "8" "H")
                       (9 "9" "D")(9 "9" "S")(9 "9" "T")(9 "9" "H")
                       (10 "10" "D")(10 "10" "S")(10 "10" "T")(10 "10" "H")
                       (10 "Jack" "D")(10 "Jack" "S")(10 "Jack" "T")(10 "Jack" "H")
                       (10 "Queen" "D")(10 "Queen" "S")(10 "Queen" "T")(10 "Queen" "H")
                       (10 "King" "D")(10 "King" "S")(10 "King" "T")(10 "King" "H")))))

;Función tomar carta
;Recibe un índice y devuelve la carta perteneciente a ese índice dentro de la baraja
(define(tomar-carta indice baraja)
  (cond((null? baraja)
        #f)
       (else
        (tomar-carta-aux indice baraja))))

(define(tomar-carta-aux indice baraja)
  (cond((equal? indice 1)
        (car baraja))
       (else
        (tomar-carta-aux (- indice 1) (cdr baraja)))))

;Función tomar carta
;Recibe una carta y elimina la aparición de la carta dentro de la baraja
(define(eliminar carta baraja)
  (cond((null? baraja)
        '())
       (else
        (eliminar-aux carta baraja))))

(define(eliminar-aux carta baraja)
  (cond((equal? carta (car baraja))
        (eliminar-aux carta (cdr baraja)))
       (else
        (cons (car baraja) (eliminar-aux carta (cdr baraja))))))

;Función barajar
;Recibe una carta y genera una nueva baraja cuyos valores serán distintos respecto a la otra
;No está al 100
(define(barajar indice baraja)
  (cond((null? baraja)
        '())
       (else
        (cons (tomar-carta indice baraja) (barajar (random 52) baraja)))))

;Función eliminados
;Recibe una carta y la baraja y elimina la aparición de esa carta en la baraja colocándola en una nueva lista
(define(eliminados carta baraja)
  (cond((null? baraja)
        (list carta))
       (else
        (cons (car carta) (eliminados carta (cdr baraja))))))

;Función eliminado?
;Recibe una carta y una lista de eliminados proveniente de la función anterior
;Verifica que la carta no esté dentro de la baraja 
(define(eliminado? carta eliminados)
  (cond((equal? (car eliminados) carta)
        #t)
       ((null? eliminados)
        #f)
       (else
        (eliminado? carta (cdr eliminados)))))

;Función puntuación
;Recibe las cartas a las cuales se les espera obtener la puntuación
;Suma los valores de las cartas y lo retorna como puntuación
(define(puntuacion cartas)
  (cond((null? cartas)
        0)
       (else
        (puntuacion-aux 0 cartas))))

(define(puntuacion-aux puntos cartas)
  (cond((null? cartas)
        puntos)
       (else
        (puntuacion-aux (+ puntos (caar cartas)) (cdr cartas)))))

;Función crupier
;Es una representación en la lógica del crupier del juego
;Recibe como parámetros su estado en el juego, las cartas que posee y su puntuación
;Retorna una lista con estos mismos parámetros una vez jugado
(define(crupier estado cartas puntuacion)
  (cond((equal? estado "Plantado")
        (append (list puntuacion) (cons (tomar-carta (random 52) (baraja 1)) cartas)))
       ((>= puntuacion 17)
        (crupier "Plantado" puntuacion cartas))
       (else
        (crupier "Jugando" (+ puntuacion (caar cartas)) (cdr (cons (tomar-carta (random 52) (baraja 1)) cartas))))))

;Función jugador
;Es una representación en la lógica del jugador
;Recibe como parámetros su estado en el juego, las cartas que posee y su puntuación
;Retorna una lista con estos mismos parámetros una vez jugado
(define(jugador estado cartas puntuacion)
  (cond((equal? estado "Plantado")
        (append (list puntuacion) (cons (tomar-carta (random 52) (baraja 1)) cartas)))
       (else
        (jugador "Jugando" (+ puntuacion (caar cartas)) (cdr (cons (tomar-carta (random 52) (baraja 1)) cartas))))))

;Función largo
;Recibe una lista y retorna su longitud
(define(largo lista)
  (cond((null? lista)
        0)
       (else
        (+ 1 (largo (cdr lista))))))

;Función ganador
;Recibe a los jugadores y al crupier
;Ejecuta una validación de los mismos, para decidir al ganador del juego
;Retorna al ganador del juego
(define(ganador jugadores crupier)
  (cond((null? jugadores)
        "Gana crupier por omisión")
       ((equal? (largo jugadores) 1)
        (cond((> (car jugadores) (car crupier))
              "Gane el jugador 1")
             (else
              "Gana el crupier")))
       ((equal? (largo jugadores) 2)
        (cond((and (> (caar jugadores) (car crupier)) (> (caar jugadores) (caadr jugadores)))
              "Gana el jugador 1")
             ((and (> (caadr jugadores) (car crupier)) (> (caadr jugadores) (caar jugadores)))
              "Gana el jugador 2")
             (else
              "Gana el crupier")))
       ((equal? (largo jugadores) 3)
        (cond((and (> (caar jugadores) (car crupier)) (> (caar jugadores) (caadr jugadores)) (> (caar jugadores) (caaddr jugadores)))
              "Gana el jugador 1")
             ((and (> (caadr jugadores) (car crupier)) (> (caadr jugadores) (caar jugadores)) (> (caadr jugadores) (caaddr jugadores)))
              "Gana el jugador 2")
             ((and (> (caaddr jugadores) (car crupier)) (> (caaddr jugadores) (caar jugadores)) (> (caaddr jugadores) (caadr jugadores)))
              "Gana el jugador 3")
             (else
              "Gana el crupier")))
       (else
        "Hay más jugadores de los permitidos en Blackjack")))
       
(provide (all-defined-out))

                               