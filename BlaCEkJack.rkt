;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname GUILenguajesBlackJack) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;#lang racket

;librerias
(require racket/gui)
(require (lib "graphics.ss" "graphics")) 
(open-graphics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Make a frame initial by instantiating the frame% class
(define frame (new frame% [label "Example"]))
 
; Make a static text message in the frame
(define msjestatico (new message% [parent frame]
                          [label "Elija la cantidad de jugadores"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Botones de la pantalla inicial donde
; se elige la cantidad de jugadores
; Make a button in the frame1 para elegir 1 jugador
(new button% [parent frame]
             [label "1 Jugador"]
             ; Callback procedure for a button click: 
             [callback (lambda (button event)                    
                         ;(send accion set-label "Escriba el nombre del jugador")
                         (send frame show #f)
                         (ventana-nombre-jugadores1 1)
                         )])
; Make a button in the frame para elegir 2 jugadores
(new button% [parent frame]
             [label "2 Jugadores"]
             ; Callback procedure for a button click: 
             [callback (lambda (button event)   
                         (send frame show #f)
                         (ventana-nombre-jugadores2 2)
                         )])
; Make a button in the frame para elegir 3 jugadores
(new button% [parent frame]
             [label "3 Jugadores"]
             ; Callback procedure for a button click: 
             [callback (lambda (button event)    
                         (send frame show #f)
                         (ventana-nombre-jugadores3 3)
                         )])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (ventana-nombre-jugadores1 cantjugadores)
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ; Make a frame by instantiating the frame% class
     ; Frame para los text field de nombre de los jugadores
     (define frame3 (new frame% [label "Nombres de jugadores "]))
    
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
     ; Text field para el nombre del o los jugadores
     (define text-field-jug1
          (new text-field%
            (label "Nombre del jugador 1: ")
            (parent frame3)
            (init-value "")) )

     ; Make a button in the frame  
     (new button% [parent frame3]
             [label "Continuar"]
             ; Callback procedure for a button click: 
             [callback (lambda (button event)
                         (send frame3 show #f)
                         (bCEj  (list (send text-field-jug1 get-value))))])
  
     ; Show the frame by calling its show method
     (send frame3 show #t)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (ventana-nombre-jugadores2 cantjugadores)
     ; Make a frame by instantiating the frame% class
     ; Frame para los text field de nombre de los jugadores
     (define frame3 (new frame% [label "Nombres de jugadores "]))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
     ; Text field para el nombre del o los jugadores      
     (define text-field-jug1    
          (new text-field%
            (label "Nombre del jugador 1: ")
            (parent frame3)
            (init-value "")) )
     (define text-field-jug2
          (new text-field%
            (label "Nombre del jugador 2: ")
            (parent frame3)
            (init-value "")))       

     ; Make a button in the frame  
     (new button% [parent frame3]
             [label "Continuar"]
             ; Callback procedure for a button click: 
             [callback (lambda (button event)
                         (send frame3 show #f)
                         (bCEj  (list (send text-field-jug1 get-value) (send text-field-jug2 get-value))))])

     ; Show the frame by calling its show method 
     (send frame3 show #t)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; funcion que abre la ventana con textfield para escribir nombre de jugadores
(define (ventana-nombre-jugadores3 cantjugadores) 
     ; Make a frame by instantiating the frame% class
     ; Frame para los text field de nombre de los jugadores
     (define frame3 (new frame% [label "Nombres de jugadores "]))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
     ; Text field para el nombre del o los jugadores
     (define text-field-jug1    
          (new text-field%
            (label "Nombre del jugador 1: ")
            (parent frame3)
            (init-value "")))
  
     (define text-field-jug2
          (new text-field%
            (label "Nombre del jugador 2: ")
            (parent frame3)
            (init-value "")))
  
     (define text-field-jug3
          (new text-field%
            (label "Nombre del jugador 3: ")
            (parent frame3)
            (init-value "")))

     ; crea un boton para continuar
     (new button% [parent frame3]
             [label "Continuar"]
             ; Callback procedure for a button click: 
             [callback (lambda (button event)
                         (send frame3 show #f)
                         (bCEj  (list (send text-field-jug1 get-value)  (send text-field-jug2 get-value) (send text-field-jug3 get-value))))])

     ; Show the frame by calling its show method
     (send frame3 show #t)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;FUNCIONES PRINCIPALES DEL JUEGO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Llama a la FUNCION PRONCIPAL
(define (bCEj X)
  (juego-blackjack X (len-lista X 0)))

;Funcion que cuenta la cantidad de elementos en una lista
(define (len-lista lista cont)
  (cond
    ((equal? lista '()) cont)
    (else
     (len-lista (cdr lista) (+ cont 1)))))


;la funcion principal del juego
;recibe una lista con los nombres y un numero con la cantidad de jugadores
(define (juego-blackjack listaNombres cantJugadores)
   
   ;::::::::::::::::::::::::::::::
   ;define y abre la ventana principal del juego
   (define ventana (open-viewport "ventana" 1040 720))
  
   ;color de "ventana" 
   ((draw-viewport ventana) "black")
   ;:::::::::::::::::::::

   ;define la ventana (llevará el fondo)
   (define ventana2 (open-pixmap "eso no importa!" 1040 720 #|tamaño de la ventana|#))
   ;:::::::::::::
   ;adicionar fondo a ventana
   ((draw-pixmap ventana2) "fondo.png" (make-posn 0.0 0.0) "black")
   ;((draw-pixmap ventana2) "cartas/card-0-0.png" (make-posn 485 600) "black")
   ;:::::::::
   ;copiamos el contenido de una ventana2 a ventana 
   (copy-viewport ventana2 ventana)
  
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;Botones
   ;;botones jugador 1
   (cond
     ((equal? 0 00)
   
   ((draw-solid-rectangle ventana2) (make-posn 470 350) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 470 350) 100 30 "black")
   ((draw-string ventana2)(make-posn 480 370 ) "PEDIR" "white")
   
   ((draw-solid-rectangle ventana2) (make-posn 470 390) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 470 390) 100 30 "black")
   ((draw-string ventana2)(make-posn 480 410) "PARAR" "white")
   )) 
   ;;botones jugador 2
   (cond
     ((equal? 0 00)
   ((draw-solid-rectangle ventana2) (make-posn 50 250) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 50 250) 100 30 "black")
   ((draw-string ventana2)(make-posn 60 270 ) "PEDIR" "white")
   
   ((draw-solid-rectangle ventana2) (make-posn 50 290) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 50 290) 100 30 "black")
   ((draw-string ventana2)(make-posn 60 310) "PARAR" "white")
   ))
 
   ;;botones jugador 3 
   (cond
     ((equal? 0 00)
   ((draw-solid-rectangle ventana2) (make-posn 900 250) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 900 250) 100 30 "black")
   ((draw-string ventana2)(make-posn 910 270 ) "PEDIR" "white")
   
   ((draw-solid-rectangle ventana2) (make-posn 900 290) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 900 290) 100 30 "black")
   ((draw-string ventana2)(make-posn 910 310) "PARAR" "white")
   ))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;muestra los nombres del o los jugadores y crupier
   ((draw-string ventana2) (make-posn 460 20)"Crupier""red") 
   (cond
     ((equal? cantJugadores 1) ((draw-string ventana2) (make-posn 485 480)(car listaNombres)"red")) ;primer nombre
     
     ((equal? cantJugadores 2) ((draw-string ventana2) (make-posn 485 480)(car listaNombres)"red");primer nombre 
                               ((draw-string ventana2) (make-posn 55 380)(car (cdr listaNombres))"red"));segundo nombre
                                    
     ((equal? cantJugadores 3) ((draw-string ventana2) (make-posn 485 480)(car listaNombres)"red");primer nombre
                               ((draw-string ventana2) (make-posn 55 380)(car (cdr listaNombres))"red");segundo nombre
                               ((draw-string ventana2) (make-posn 905 380)(car (cdr (cdr listaNombres)))"red")));tercer nombre
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (copy-viewport ventana2 ventana)

 
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
   ;(define (carta-seleccionada valor simbolo)
     
   ;funcion que coloca cada imagen en las cordenadas especificadas
   (define (coloca-imagen imagen cordx cordy jugador)
     ((equal? jugador 1) (draw-pixmap ventana2) imagen (make-posn cordx cordy) "black"))
     
   ;:::::::::::::::::::::::::::::::::::::::::::::::
   ;actualiza la ventana con los nuevos cambios 
   (copy-viewport ventana2 ventana)
   ;::::::::::::::::::::::::::::::::::::::::::::::::::

  ;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::::
  ;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::::
  ;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::::
  ;Funcion que tiene control de cada click
  ;La listaconinformacion es una lista donde se guarda la info de cada jugador necesaria para ir verificando
  ;en cada turno hasta que alguien gane o pierda
  (define (control click listaconinformacion) 
    (cond
      ;***************Botones Jugador 1**************************************
      ;;Boton Pedir
      ((and (>= (posn-x (mouse-click-posn click)) 470) (<= (posn-x (mouse-click-posn click))570)
            (>= (posn-y (mouse-click-posn click))350)  (<= (posn-y (mouse-click-posn click))380))
       (begin
            ;llama a funcion de pedir carta para dibujarla
            ((draw-pixmap ventana2) "cartas/card-0-0.png" (make-posn 485 600) "black")
            (copy-viewport ventana2 ventana)
            (control (get-mouse-click ventana) listaconinformacion)  
            ))
      
           
      ;;Boton Plantarse
      ((and (>= (posn-x (mouse-click-posn click)) 470) (<= (posn-x (mouse-click-posn click))570)
            (>= (posn-y (mouse-click-posn click))390)  (<= (posn-y (mouse-click-posn click))420))
       (begin
            ;llama a funcion de plantarse
            ((draw-pixmap ventana2) "cartas/card-0-1.png" (make-posn 485 600) "black")
            (copy-viewport ventana2 ventana)
            (control (get-mouse-click ventana) listaconinformacion)
            )) 




      
      ;***************Botones Jugador 2************************************** 
      ;;Boton Pedir
      ((and (>= (posn-x (mouse-click-posn click)) 50) (<= (posn-x (mouse-click-posn click))150)
            (>= (posn-y (mouse-click-posn click))250)  (<= (posn-y (mouse-click-posn click))280))
       (begin
            (close-viewport ventana)
            (close-graphics)))
      ;;Boton Plantarse
      ((and (>= (posn-x (mouse-click-posn click)) 50) (<= (posn-x (mouse-click-posn click))150)
            (>= (posn-y (mouse-click-posn click))290)  (<= (posn-y (mouse-click-posn click))320))
       (begin
            (close-viewport ventana)
            (close-graphics)))
      
      ;***************Botones Jugador 3**************************************
      ;;Boton Pedir
      ((and (>= (posn-x (mouse-click-posn click)) 900) (<= (posn-x (mouse-click-posn click))1000)
            (>= (posn-y (mouse-click-posn click))250)  (<= (posn-y (mouse-click-posn click))280))
       (begin
            (close-viewport ventana)
            (close-graphics)))
      ;;Boton Plantarse
      ((and (>= (posn-x (mouse-click-posn click)) 900) (<= (posn-x (mouse-click-posn click))1000)
            (>= (posn-y (mouse-click-posn click))290)  (<= (posn-y (mouse-click-posn click))320))
       (begin
            (close-viewport ventana)
            (close-graphics)))
      


      
      (else (control (get-mouse-click ventana) listaconinformacion))
    )
   )
  (control (get-mouse-click ventana) '())
  ;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::::
  ;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::::
  ;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::::


  
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Show the frame by calling its show method
(send frame show #t)