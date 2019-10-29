;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname BlaCEkJack) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;librerias
(require racket/gui)
(require (lib "graphics.ss" "graphics"))
(require "Blackjack.rkt")
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






;*******************************************************************************************
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

;*******************************************************************************************
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

;*******************************************************************************************
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;FUNCIONES PRINCIPALES DEL JUEGO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Funcion principal
; Recibe una lista con el nombre de los jugadores
; Y llama a otra funcion donde le envia la lista
; y len de la lista (cantidad de jugadores en la lista)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bCEj X)
  (juego-blackjack X (len-lista X 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funcion que cuenta la cantidad de elementos en una lista
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (len-lista lista cont)
  (cond
    ((equal? lista '()) cont)
    (else
     (len-lista (cdr lista) (+ cont 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; la funcion complementaria o auxiliar a la principal
; recibe una lista con los nombres de los jugadores
; y la cantidad de jugadores en esa lista
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (juego-blackjack listaNombres cantJugadores)
   
  
   
   (define ventana (open-viewport "ventana" 1040 720))                                  ;define y abre la ventana principal del juego
   ;::::::::::::::::::::: 
   ((draw-viewport ventana) "black")                                                    ;color de "ventana" 
   ;:::::::::::::::::::::
   (define ventana2 (open-pixmap "eso no importa!" 1040 720 #|tamaño de la ventana|#))  ;define la ventana auxiliar (llevará el fondo)
   ;:::::::::::::::::::::
   ((draw-pixmap ventana2) "fondo.png" (make-posn 0.0 0.0) "black")                     ;adicionar fondo a la ventana auxiliar
   ;:::::::::::::::::::::
   (copy-viewport ventana2 ventana)                                                     ;copiamos el contenido de una ventana2 a ventana
  







  
   ;************************************************************************************
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Botones;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;;;;;;;;;;;;;;;;;;;;;
   ;; Botones jugador 1
   (cond
     ((>= cantJugadores 1)
   
   ((draw-solid-rectangle ventana2) (make-posn 470 350) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 470 350) 100 30 "black")
   ((draw-string ventana2)(make-posn 480 370 ) "PEDIR" "white")
   
   ((draw-solid-rectangle ventana2) (make-posn 470 390) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 470 390) 100 30 "black")
   ((draw-string ventana2)(make-posn 480 410) "PLANTARSE" "white")
   ))
  
   ;;;;;;;;;;;;;;;;;;;;;
   ;; Botones jugador 2
   (cond
     ((>= cantJugadores 2)
   ((draw-solid-rectangle ventana2) (make-posn 50 250) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 50 250) 100 30 "black")
   ((draw-string ventana2)(make-posn 60 270 ) "PEDIR" "white")
   
   ((draw-solid-rectangle ventana2) (make-posn 50 290) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 50 290) 100 30 "black")
   ((draw-string ventana2)(make-posn 60 310) "PLANTARSE" "white")
   ))
  
   ;;;;;;;;;;;;;;;;;;;;;
   ;; Botones jugador 3 
   (cond
     ((equal? cantJugadores 3)
   ((draw-solid-rectangle ventana2) (make-posn 900 250) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 900 250) 100 30 "black")
   ((draw-string ventana2)(make-posn 910 270 ) "PEDIR" "white")
   
   ((draw-solid-rectangle ventana2) (make-posn 900 290) 100 30 "gray")
   ((draw-rectangle ventana2) (make-posn 900 290) 100 30 "black")
   ((draw-string ventana2)(make-posn 910 310) "PLANTARSE" "white")
   ))








  
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Muestra los nombres de los jugadores;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;y crupier;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
   ((draw-string ventana2) (make-posn 460 20)"Crupier""red") 
   (cond
     ((equal? cantJugadores 1) ((draw-string ventana2) (make-posn 485 480)(car listaNombres)"red"))              ;nombre jugador 1
     
     ((equal? cantJugadores 2) ((draw-string ventana2) (make-posn 485 480)(car listaNombres)"red")               ;nombre jugador 1 
                               ((draw-string ventana2) (make-posn 55 380)(car (cdr listaNombres))"red"))         ;nombre jugador 2
                                    
     ((equal? cantJugadores 3) ((draw-string ventana2) (make-posn 485 480)(car listaNombres)"red")               ;nombre jugador 1
                               ((draw-string ventana2) (make-posn 55 380)(car (cdr listaNombres))"red")          ;nombre jugador 2
                               ((draw-string ventana2) (make-posn 905 380)(car (cdr (cdr listaNombres)))"red"))) ;nombre jugador 3
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (copy-viewport ventana2 ventana) ;Actualiza la ventana






  
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
   ; Funcion que recibe en una lista con informacion necesaria de
   ; una imagen para cargarla y colocarla en cordenadas x,y especificadas
   (define (colocar-imagen imagen cordx cordy)
     (cond
       ((equal? (car (cdr (cdr imagen))) "D") (aux-colocar-imagen (~r (- (car imagen) 1)) (~r 1) cordx cordy))
       ((equal? (car (cdr (cdr imagen))) "H") (aux-colocar-imagen (~r (- (car imagen) 1)) (~r 2) cordx cordy))
       ((equal? (car (cdr (cdr imagen))) "T") (aux-colocar-imagen (~r (- (car imagen) 1)) (~r 3) cordx cordy))
       ((equal? (car (cdr (cdr imagen))) "S") (aux-colocar-imagen (~r (- (car imagen) 1)) (~r 4) cordx cordy))))
  
   (define (aux-colocar-imagen strvalor strsimbolo cordx cordy)
     ((draw-pixmap ventana2) (string-append "cartas/card-" strvalor "-" strsimbolo ".png")   (make-posn cordx cordy) "black"))  
            
   ;::::::::::::::::::::::::::::::::::::::::::::::: 
   (copy-viewport ventana2 ventana);actualiza la ventana 
   ;::::::::::::::::::::::::::::::::::::::::::::::::::







  
  ;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::::
  ;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::::
  ;:::::::::::::::::::::::::::::::::::::::::::::::::;::::::::::::::::::::::::::::::::::::::::::::::::: 
  ;Funcion que tiene control de cada click
  ;La listaconinformacion es una lista donde se guarda la info de cada jugador necesaria para ir verificando
  ;en cada turno hasta que alguien gane o pierda
  (define (control click matriz puntuacion cantPlantes) 
    (cond
      
      ;***************Botones Jugador 1**************************************
      ;;Boton Pedir
      ((and (>= (posn-x (mouse-click-posn click)) 470) (<= (posn-x (mouse-click-posn click))570)
            (>= (posn-y (mouse-click-posn click))350)  (<= (posn-y (mouse-click-posn click))380))
       (begin
            ;llama a funcion de pedir carta para dibujarla
            ;
            (cond((>= (cadr puntuacion) 17)
               (control (get-mouse-click ventana) (list (tomar-carta (random 52) (baraja 1))) (list (+ puntuacion (caar matriz)) puntuacion) cantPlantes))
              (else
               (control (get-mouse-click ventana) (list (tomar-carta (random 52) (baraja 1)) (tomar-carta (random 52) (baraja 1))) (list (+ puntuacion (caar matriz)) (+ puntuacion (caadr matriz))) cantPlantes)))
            (colocar-imagen (car matriz) (+ (caar matriz) 10) (+ (caar matriz) 10))
            (copy-viewport ventana2 ventana)  
            ))
        
           
      ;;Boton Plantarse
      ((and (>= (posn-x (mouse-click-posn click)) 470) (<= (posn-x (mouse-click-posn click))570)
            (>= (posn-y (mouse-click-posn click))390)  (<= (posn-y (mouse-click-posn click))420))
       (begin
            ;llama a funcion de plantarse
            ;((draw-pixmap ventana2) "cartas/card-0-1.png" (make-posn 485 600) "black")
            ; si equal cant plantes es igual al cant jugs pasar a siguiente ventantana
            (copy-viewport ventana2 ventana)
            (control (get-mouse-click ventana) (list matriz (tomar-carta (random 52) (baraja 1))) (+ cantPlantes 1))  
            ))   


 




      
      
      ;***************Botones Jugador 2************************************** 
      ;;Boton Pedir
      ((and (>= (posn-x (mouse-click-posn click)) 50) (<= (posn-x (mouse-click-posn click))150)
            (>= (posn-y (mouse-click-posn click))250)  (<= (posn-y (mouse-click-posn click))280))
       (begin
        (cond((>= (caddr puntuacion) 17)
               (control (get-mouse-click ventana) (list (tomar-carta (random 52) (baraja 1)) (tomar-carta (random 52) (baraja 1))) (list (+ puntuacion (caar matriz)) (+ puntuacion (caadr matriz)) puntuacion) cantPlantes))
              (else
               (control (get-mouse-click ventana) (list (tomar-carta (random 52) (baraja 1)) (tomar-carta (random 52) (baraja 1)) (tomar-carta (random 52) (baraja 1))) (list (+ puntuacion (caar matriz)) (+ puntuacion (caadr matriz)) (+ puntuacion (caaddr matriz))) cantPlantes)))
            (colocar-imagen (car matriz) (+ (caar matriz) 10) (+ (caar matriz) 10))
            (colocar-imagen (cadr matriz) (+ (caadr matriz) 100) (+ (caadr matriz) 100))
            (close-viewport ventana)
            (close-graphics)))
      ;;Boton Plantarse
      ((and (>= (posn-x (mouse-click-posn click)) 50) (<= (posn-x (mouse-click-posn click))150)
            (>= (posn-y (mouse-click-posn click))290)  (<= (posn-y (mouse-click-posn click))320))
       (begin
         (control (get-mouse-click ventana) (list matriz (tomar-carta (random 52) (baraja 1))) 1) 
            (close-viewport ventana)
            (close-graphics)))











      
      ;***************Botones Jugador 3**************************************
      ;;Boton Pedir
      ((and (>= (posn-x (mouse-click-posn click)) 900) (<= (posn-x (mouse-click-posn click))1000)
            (>= (posn-y (mouse-click-posn click))250)  (<= (posn-y (mouse-click-posn click))280))
       (begin
            ;Verifica si aun puede pedir cartas, si puede procede, si no puede, manda un mensaje diciendo que no puede y hace la recursividad
         (cond((>= (cadddr puntuacion) 17)
               (control (get-mouse-click ventana) (list (tomar-carta (random 52) (baraja 1)) (tomar-carta (random 52) (baraja 1)) (tomar-carta (random 52) (baraja 1))) (list (+ puntuacion (caar matriz)) (+ puntuacion (caadr matriz)) (+ puntuacion (caaddr matriz)) puntuacion) cantPlantes)
               (colocar-imagen (car matriz) (+ (caar matriz) 10) (+ (caar matriz) 10))
               (colocar-imagen (cadr matriz) (+ (caadr matriz) 100) (+ (caadr matriz) 100))
               (colocar-imagen (caddr matriz) (+ (caaddr matriz) 200) (+ (caaddr matriz) 200)))
              (else
               (control (get-mouse-click ventana) (list (tomar-carta (random 52) (baraja 1)) (tomar-carta (random 52) (baraja 1)) (tomar-carta (random 52) (baraja 1)) (tomar-carta (random 52) (baraja 1))) (list (+ puntuacion (caar matriz)) (+ puntuacion (caadr matriz)) (+ puntuacion (caaddr matriz)) (+ puntuacion (car(cadddr matriz))) cantPlantes)))
            
            ;(close-viewport ventana)
            ;(close-graphics)
            )))
      ;;Boton Plantarse
      ((and (>= (posn-x (mouse-click-posn click)) 900) (<= (posn-x (mouse-click-posn click))1000)
            (>= (posn-y (mouse-click-posn click))290)  (<= (posn-y (mouse-click-posn click))320))
       (begin
         (control (get-mouse-click ventana) (list matriz (tomar-carta (random 52) (baraja 1))) 1) 
            ))

      ((equal? cantJugadores cantPlantes)
       (pantalla-final puntuacion listaNombres ganador cantJugadores)
            (close-viewport ventana))
       
      

      
      (else (control (get-mouse-click ventana) matriz puntuacion cantPlantes)))
   ) ;(control click
  
  (control (get-mouse-click ventana) '() '() 0)
  ;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::
)






;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::;:::::::::::::::::::::::::::::::::::::::::::::::::



(define (pantalla-final puntuacion listaNombres ganador cantJugadores)
  (define ventana (open-viewport "ventana Puntajes" 300 380))                           ;define y abre la ventana 
   ;::::::::::::::::::::: 
   ((draw-viewport ventana) "black")                                                    ;color de "ventana" 
   ;:::::::::::::::::::::
   (define ventana2 (open-pixmap "eso no importa!" 300 380 #|tamaño de la ventana|#))   ;define la ventana auxiliar (llevará el fondo)
   ;:::::::::::::::::::::
   ((draw-pixmap ventana2) "fondonegro.jpg" (make-posn 0.0 0.0) "black")                ;adicionar fondo a la ventana auxiliar
   ;:::::::::::::::::::::
   (copy-viewport ventana2 ventana)                                                     ;copiamos el contenido de una ventana2 a ventana

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Muestra los nombres de los jugadores;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;y crupier;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
   ((draw-string ventana2) (make-posn 70 50)"SE ACABÓ EL JUEGO!!!""red")
   ((draw-string ventana2) (make-posn 20 110) (string-append "EL GANADOR ES: " ganador) "red")

   
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Muestra los nombres de los jugadores;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;y crupier;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
   (cond
     ((equal? cantJugadores 1) ((draw-string ventana2) (make-posn 20 200)(string-append (car listaNombres) ": "  (~r(car puntuacion)))"red")                       ;nombre y puntuacion jugador 1
                               
                               ((draw-string ventana2) (make-posn 20 250)(string-append "CRUPIER: " (~r(car (cdr (cdr(cdr puntuacion))))))  "red")                 ;puntuacion crupier
                               )
     
     ((equal? cantJugadores 2) ((draw-string ventana2) (make-posn 20 200)(string-append (car listaNombres) ": "  (~r(car puntuacion)))"red")                       ;nombre y puntuacion jugador 1
                                           
                               ((draw-string ventana2) (make-posn 20 250)(string-append (car (cdr listaNombres)) ": " (~r(car (cdr puntuacion))))"red")            ;nombre y puntuacion jugador 2
                               
                               ((draw-string ventana2) (make-posn 20 300)(string-append "CRUPIER: " (~r(car (cdr (cdr(cdr puntuacion))))))  "red")                 ;puntuacion crupier
                               )
                                    
     ((equal? cantJugadores 3) ((draw-string ventana2) (make-posn 20 200)(string-append (car listaNombres) ": "  (~r(car puntuacion)))"red")                       ;nombre y puntuacion jugador 1
                                           
                               ((draw-string ventana2) (make-posn 20 250)(string-append (car (cdr listaNombres)) ": " (~r(car (cdr puntuacion))))"red")            ;nombre y puntuacion jugador 2
                               
                               ((draw-string ventana2) (make-posn 20 300) (string-append (car (cdr (cdr listaNombres))) (~r(car (cdr (cdr puntuacion))))) "red")   ;nombre y puntuacion jugador 3
                              
                               ((draw-string ventana2) (make-posn 20 350)(string-append "CRUPIER: " (~r(car (cdr (cdr(cdr puntuacion))))))  "red")                 ;puntuacion crupier
                               ) 
   )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (copy-viewport ventana2 ventana) ;Actualiza la ventana

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Show the frame by calling its show method
(send frame show #t)