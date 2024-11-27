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

(provide (all-defined-out))

(define-struct game (snake-body head score))
;; Game is (make-game  (listof cells) score)
;; interp. the current state of a snake game
;;         with the current cells of the snake body, the position of the head of the snake
;;         and score

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
                   (cond [(= head 0) (if (= acc 1) 
                                         (place-image/align (square CELL_WIDTH "solid" "seagreen")
                                                            (* (remainder (car body) COUNT_CELLS) CELL_WIDTH)
                                                            (* (quotient (car body) COUNT_CELLS) CELL_WIDTH)
                                                            "left" "top" (aux (cdr body) head bg (add1 acc)))
                                         (place-image/align (square CELL_WIDTH "solid" "green")
                                                            (* (remainder (car body) COUNT_CELLS) CELL_WIDTH)
                                                            (* (quotient (car body) COUNT_CELLS) CELL_WIDTH)
                                                            "left" "top" (aux (cdr body) head bg (add1 acc))))]
                         [(= head 1) (if (= acc (length body0))
                                         (place-image/align (square CELL_WIDTH "solid" "seagreen")
                                                            (* (remainder (car body) COUNT_CELLS) CELL_WIDTH)
                                                            (* (quotient (car body) COUNT_CELLS) CELL_WIDTH)
                                                            "left" "top" (aux (cdr body) head bg (add1 acc)))
                                         (place-image/align (square CELL_WIDTH "solid" "green")
                                                            (* (remainder (car body) COUNT_CELLS) CELL_WIDTH)
                                                            (* (quotient (car body) COUNT_CELLS) CELL_WIDTH)
                                                            "left" "top" (aux (cdr body) head bg (add1 acc))))])]))]
    (aux body0 head bg 1))
  )

(define (main score)
  (big-bang score
    (to-draw render)
    ))