#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)


(define SCENE_WIDTH 700)
(define SCENE_HEIGHT 745)
(define SCORE_STRING "Score: ")
(define SCORE_SIZE 20)
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
;(define start (random 700))

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
  (local [(define dir (get-direction (game-snake-body g)))]
    (cond [(= dir RIGHT)(game (cons (add1 (car (game-snake-body g))) 
                                    (take (game-snake-body g)
                                          (sub1 (length (game-snake-body g))))) (game-score g))]
          [(= dir LEFT) (game (cons (sub1 (car (game-snake-body g))) 
                                    (take (game-snake-body g)
                                          (sub1 (length (game-snake-body g))))) (game-score g))]
          [(= dir UP) (game (cons (- (car (game-snake-body g)) COUNT_CELLS) 
                                  (take (game-snake-body g)
                                        (sub1 (length (game-snake-body g))))) (game-score g))]
          [(= dir DOWN) (game (cons (+ (car (game-snake-body g)) COUNT_CELLS) 
                                    (take (game-snake-body g)
                                          (sub1 (length (game-snake-body g))))) (game-score g))]
          )))

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
  (cond [(and (or (= (get-direction (game-snake-body g)) RIGHT) (= (get-direction (game-snake-body g)) LEFT)) (key=? a-key "up"))
         (game (cons (- (car (game-snake-body g)) COUNT_CELLS)(take (game-snake-body g) (sub1 (length (game-snake-body g))))) (game-score g))]
        [(and (or (= (get-direction (game-snake-body g)) RIGHT) (= (get-direction (game-snake-body g)) LEFT)) (key=? a-key "down"))
         (game (cons (+ (car (game-snake-body g)) COUNT_CELLS)(take (game-snake-body g) (sub1 (length (game-snake-body g))))) (game-score g))]
        [(and (or (= (get-direction (game-snake-body g)) DOWN) (= (get-direction (game-snake-body g)) UP)) (key=? a-key "right"))
         (game (cons (add1 (car (game-snake-body g)))(take (game-snake-body g) (sub1 (length (game-snake-body g))))) (game-score g))]
        [(and (or (= (get-direction (game-snake-body g)) DOWN) (= (get-direction (game-snake-body g)) UP)) (key=? a-key "left"))
         (game (cons (sub1 (car (game-snake-body g)))(take (game-snake-body g) (sub1 (length (game-snake-body g))))) (game-score g))]
        [else g]
        ))

(define (main score)
  (big-bang score
    (to-draw render)
    (on-tick next 0.2)
    (on-key handle-arrows)
    ))

