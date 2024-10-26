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
(define GRID_BORDER_COLOR "black")
(define BACKGROUND (empty-scene SCENE_WIDTH SCENE_HEIGHT))

(provide (all-defined-out))

(define (render score) (place-images/align (list (text (string-append SCORE_STRING (number->string score))
                                                       SCORE_SIZE SCORE_COLOR)
                                                 (rectangle GRID_WIDTH GRID_HEIGHT "outline" GRID_BORDER_COLOR))
                                           (list (make-posn SCORE_X SCORE_Y)
                                                 (make-posn GRID_X GRID_Y))
                                           "left" "top" BACKGROUND))

(define (main score)
  (big-bang score
    (to-draw render)
    ))