#lang racket

(require rackunit)
(require 2htdp/image)
(require "ccsnake.rkt")

(check-equal? (render 0) (place-image/align (text SCORE_STRING SCORE_SIZE SCORE_COLOR) SCORE_X SCORE_Y "left" "top" BACKGROUND))