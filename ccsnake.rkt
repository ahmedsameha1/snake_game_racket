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

(provide (all-defined-out))

(struct game (snake-body head dir score) #:transparent)
;; Game is (make-game  (listof cells) score)
;; interp. the current state of a snake game
;;         with the current cells of the snake body, the position of the head of the snake,
;;         the direction of the move of the snake and score

(define (render g) (place-images/align (list (text (string-append SCORE_STRING (number->string (game-score g)))
                                                   SCORE_SIZE SCORE_COLOR)
                                             (rectangle GRID_WIDTH GRID_HEIGHT "outline" GRID_BORDER_COLOR)
                                             (draw-snake-body (game-snake-body g) (game-head g) (empty-scene GRID_WIDTH GRID_WIDTH)))      
                                       (list (make-posn SCORE_X SCORE_Y)
                                             (make-posn GRID_X GRID_Y)
                                             (make-posn GRID_X GRID_Y))
                                       "left" "top" BACKGROUND))

;; List of Numbers, Image -> Image
;; Draws the body of the snake over the image of background
(define (draw-snake-body body0 head bg)
  (local [(define (aux body head bg acc)
            (cond [(empty? body) bg]
                  [else
                   (cond [(= head HEAD_START) (if (= acc 1) 
                                                  (place-image/align (square CELL_WIDTH "solid" "seagreen")
                                                                     (* (col-of-cell (car body)) CELL_WIDTH)
                                                                     (* (quotient (car body) COUNT_CELLS) CELL_WIDTH)
                                                                     "left" "top" (aux (cdr body) head bg (add1 acc)))
                                                  (place-image/align (square CELL_WIDTH "solid" "green")
                                                                     (* (col-of-cell (car body)) CELL_WIDTH)
                                                                     (* (row-of-cell (car body)) CELL_WIDTH)
                                                                     "left" "top" (aux (cdr body) head bg (add1 acc))))]
                         [(= head HEAD_END) (if (= acc (length body0))
                                                (place-image/align (square CELL_WIDTH "solid" "seagreen")
                                                                   (* (col-of-cell (car body)) CELL_WIDTH)
                                                                   (* (row-of-cell (car body)) CELL_WIDTH)
                                                                   "left" "top" (aux (cdr body) head bg (add1 acc)))
                                                (place-image/align (square CELL_WIDTH "solid" "green")
                                                                   (* (col-of-cell (car body)) CELL_WIDTH)
                                                                   (* (row-of-cell (car body)) CELL_WIDTH)
                                                                   "left" "top" (aux (cdr body) head bg (add1 acc))))])]))]
    (aux body0 head bg 1)))

;; game -> game
;; create the next game state
(define (next g)
  (cond [(and (= (game-dir g) RIGHT) (= (game-head g) HEAD_END))
         (game (map (lambda (cell) (add1 cell)) (game-snake-body g)) (game-head g) (game-dir g) (game-score g))]
        [(and (= (game-dir g) LEFT) (= (game-head g) HEAD_START))
         (game (map (lambda (cell) (sub1 cell)) (game-snake-body g)) (game-head g) (game-dir g) (game-score g))]
        [(and (= (game-dir g) UP) (= (game-head g) HEAD_START))
         (game (map (lambda (cell) (- cell COUNT_CELLS)) (game-snake-body g)) (game-head g) (game-dir g) (game-score g))]
        [(and (= (game-dir g) DOWN) (= (game-head g) HEAD_END))
         (game (map (lambda (cell) (+ cell COUNT_CELLS)) (game-snake-body g)) (game-head g) (game-dir g) (game-score g))]
        [(and (= (game-dir g) RIGHT)
              (= (game-head g) HEAD_START)
              (> (col-of-cell (add1 (car (game-snake-body g))))
                 (col-of-cell (car (cdr (game-snake-body g))))))
         (game (move-right (game-snake-body g)) (game-head g) (game-dir g) (game-score g))]
        [(and (= (game-dir g) RIGHT)
              (= (game-head g) HEAD_START) (error "Illegal move"))]
        [else g]))

; listof cells -> listof cells
; move the body of the snake to the right direction
(define (move-right cl) 
  (local [(define (aux cl prev-row prev-col)
            (cond [(empty? cl) empty]
                  [else (cons 
                         (cond [(or (= (row-of-cell (car cl)) prev-row) (= prev-row -1)) (add1 (car cl))]
                               [(or (= (col-of-cell (car cl)) prev-col) (= prev-col -1)) (- (car cl) COUNT_CELLS)]
                               [ (= (row-of-cell (car cl)) (row-of-cell (car (cdr cl)))) (add1 (car cl))]
                               [ (= (col-of-cell (car cl)) (add1 prev-col)) (- (car cl) COUNT_CELLS)])
                         (aux (cdr cl) (row-of-cell (car cl)) (col-of-cell (car cl))))]))]
    (aux cl -1 -1)))

; number -> number
; get the row of the cell
(define (row-of-cell c)
  (quotient c COUNT_CELLS))

; number -> number
; get the column of the cell
(define (col-of-cell c)
  (remainder c COUNT_CELLS))


(define (main score)
  (big-bang score
    (to-draw render)
    ))