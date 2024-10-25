#lang racket

(require 2htdp/image)
(require 2htdp/universe)


(define SCENE_WIDTH 700)
(define SCENE_HEIGHT 700)
(define SCORE_STRING "Score: ")
(define SCORE_SIZE 20)
(define SCORE_COLOR "green")
(define SCORE_X 15)
(define SCORE_Y 15)
(define BACKGROUND (empty-scene SCENE_WIDTH SCENE_HEIGHT))

(provide (all-defined-out))

(define (render score) (place-image/align (text SCORE_STRING SCORE_SIZE SCORE_COLOR) SCORE_X SCORE_Y "left" "top" BACKGROUND))

(define (main score)
  (big-bang score
    (to-draw render)
    ))