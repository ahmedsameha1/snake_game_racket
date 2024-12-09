#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)


(define SCENE_WIDTH 700)
(define SCENE_HEIGHT 745)
(define SCORE_STRING "Score: ")
(define SCORE_SIZE 20)
(define RESULT_SCORE_SIZE (* 3 SCORE_SIZE))
(define SCORE_COLOR "brown")
(define SCORE_X 15)
(define SCORE_Y 15)
(define GRID_X 0)
(define GRID_Y 45)
(define GRID_WIDTH 700)
(define GRID_HEIGHT 700)
(define CELL_WIDTH 20)
(define COUNT_CELLS (/ GRID_WIDTH CELL_WIDTH))
(define GRID_BORDER_COLOR "black")
(define BACKGROUND (empty-scene SCENE_WIDTH SCENE_HEIGHT))
(define RIGHT 0)
(define DOWN 1)
(define LEFT 2)
(define UP 3)
(define HEAD_START 0)
(define HEAD_END 1)
(define START_NEW_GAME "Press space to start a new game")

(provide (all-defined-out))

(struct game (snake-body score) #:transparent)
;; Game is (make-game  (listof cells) score)
;; interp. the current state of a snake game
;;         with the current cells of the snake body and score

;; game -> Image
;; graw the game state on ticks
(define (render g) (place-images/align (list (text (string-append SCORE_STRING (number->string (game-score g)))
                                                   SCORE_SIZE SCORE_COLOR)
                                             (rectangle GRID_WIDTH GRID_HEIGHT "outline" GRID_BORDER_COLOR)
                                             (draw-snake-body (game-snake-body g) (empty-scene GRID_WIDTH GRID_WIDTH)))      
                                       (list (make-posn SCORE_X SCORE_Y)
                                             (make-posn GRID_X GRID_Y)
                                             (make-posn GRID_X GRID_Y))
                                       "left" "top" BACKGROUND))

;; List of Numbers, Image -> Image
;; Draws the body of the snake over the image of background
(define (draw-snake-body body0 bg)
  (local [(define (aux body bg)
            (cond [(empty? body) bg]
                  [else  (place-image/align (square CELL_WIDTH "solid" "green")
                                            (* (col-of-cell (car body)) CELL_WIDTH)
                                            (* (row-of-cell (car body)) CELL_WIDTH)
                                            "left" "top" (aux (cdr body) bg))]))]
    (place-image/align (square CELL_WIDTH "solid" "seagreen")
                       (* (col-of-cell (car body0)) CELL_WIDTH)
                       (* (row-of-cell (car body0)) CELL_WIDTH)
                       "left" "top" (aux (cdr body0) bg))))

;; game -> game
;; create the next game state
(define (next g)
  (local [(define dir (get-direction (game-snake-body g)))
          (define head (car (game-snake-body g)))
          (define body (game-snake-body g))
          (define score (game-score g))]
    (cond [(= dir RIGHT)(game (cons (add1 head) (take body (sub1 (length body)))) score)]
          [(= dir LEFT) (game (cons (sub1 head) (take body (sub1 (length body)))) score)]
          [(= dir UP) (game (cons (- head COUNT_CELLS) (take body (sub1 (length body)))) score)]
          [(= dir DOWN) (game (cons (+ head COUNT_CELLS) (take body (sub1 (length body)))) score)])))


;; listof cells -> number
;; get the direction of the body of the snake
;; given that the first cell share the same row or column with the second cell
(define (get-direction cl)
  (if (= (row-of-cell (car cl)) (row-of-cell (car (cdr cl))))
      (if (> (col-of-cell (car cl)) (col-of-cell (car (cdr cl)))) RIGHT LEFT)
      (if (> (row-of-cell (car cl)) (row-of-cell (car (cdr cl)))) DOWN UP)))

; number -> number
; get the row of the cell
(define (row-of-cell c)
  (quotient c COUNT_CELLS))

; number -> number
; get the column of the cell
(define (col-of-cell c)
  (remainder c COUNT_CELLS))

;; game, string (key) -> game
;; handle the user input of arrow keys
(define (handle-arrows g a-key)
  (local [(define dir (get-direction (game-snake-body g)))
          (define head  (car (game-snake-body g)))
          (define body (game-snake-body g))
          (define score (game-score g))]
    (cond [(and (or (= dir RIGHT) (= dir LEFT)) (key=? a-key "up"))
           (game (cons (- head COUNT_CELLS)(take body (sub1 (length body)))) score)]
          [(and (or (= dir RIGHT) (= dir LEFT)) (key=? a-key "down"))
           (game (cons (+ head COUNT_CELLS)(take body (sub1 (length body)))) score)]
          [(and (or (= dir DOWN) (= dir UP)) (key=? a-key "right"))
           (game (cons (add1 head)(take body (sub1 (length body)))) score)]
          [(and (or (= dir DOWN) (= dir UP)) (key=? a-key "left"))
           (game (cons (sub1 head)(take body (sub1 (length body)))) score)]
          [else g])))


;; get randomly the head of the body of the snake at the start of the game 
(define (get-starting-head random)
  (local [(define head (random 1225))]
    (if (or (< (row-of-cell head) 3) (> (row-of-cell head) 31) (< (col-of-cell head) 3) (> (col-of-cell head) 31)) 
        (get-starting-head random)
        head)))

(define (get-starting-game get-starting-head get-starting-direction)
  (local [(define starting-snake-head (get-starting-head random))
          (define direction (get-starting-direction 4))]
    (cond [(= direction RIGHT) (game (list starting-snake-head (- starting-snake-head 1) (- starting-snake-head 2) (- starting-snake-head 3)) 0)]
          [(= direction LEFT) (game (list starting-snake-head (+ starting-snake-head 1) (+ starting-snake-head 2) (+ starting-snake-head 3)) 0)]
          [(= direction DOWN) (game (list starting-snake-head (- starting-snake-head 35) (- starting-snake-head 70) (- starting-snake-head 105)) 0)]
          [(= direction UP) (game (list starting-snake-head (+ starting-snake-head 35) (+ starting-snake-head 70) (+ starting-snake-head 105)) 0)])))

(define (game-over? g) 
  (local [(define col1 (col-of-cell (car (game-snake-body g))))
          (define row1 (row-of-cell (car (game-snake-body g))))
          (define col2 (col-of-cell (car (cdr (game-snake-body g)))))
          (define row2 (row-of-cell (car (cdr (game-snake-body g)))))]
    (or (< row1 0) (> row1 34) (< col1 0) (> col1 34) (and (not (= row1 row2)) (not (= col1 col2))))))

(define (render-result g) 
  (local [(define result (above 
                          (text (string-append SCORE_STRING (number->string (game-score g))) RESULT_SCORE_SIZE SCORE_COLOR)
                          (text START_NEW_GAME SCORE_SIZE SCORE_COLOR)))]
    (place-image/align result (/ (- (image-width BACKGROUND) (image-width result)) 2)
                       (/ (- (image-height BACKGROUND) (image-height result)) 2)
                       "left" "top" BACKGROUND)))

(define (main g)
  (big-bang (big-bang g
              (to-draw render)
              (on-tick next 0.2)
              (on-key handle-arrows)
              (stop-when game-over?)
              (close-on-stop 1))
    (to-draw render-result)))

(main (get-starting-game get-starting-head random))