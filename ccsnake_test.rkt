#lang racket

(require rackunit)
(require 2htdp/image)
(require lang/posn)
(require "ccsnake.rkt")

(check-equal? (render 0) (place-images/align (list (text (string-append SCORE_STRING (number->string 0))
                                                         SCORE_SIZE SCORE_COLOR)
                                                   (rectangle GRID_WIDTH GRID_HEIGHT "outline" GRID_BORDER_COLOR))
                                             (list (make-posn SCORE_X SCORE_Y)
                                                   (make-posn GRID_X GRID_Y))
                                             "left" "top" BACKGROUND))