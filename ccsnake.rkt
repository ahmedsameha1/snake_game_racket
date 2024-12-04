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

(struct game (snake-body dir score) #:transparent)
;; Game is (make-game  (listof cells) score)
;; interp. the current state of a snake game
;;         with the current cells of the snake body
;;         the direction of the move of the snake and score

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
  (local [(define (aux body bg acc)
            (cond [(empty? body) bg]
                  [else
                   (if (= acc 1) 
                       (place-image/align (square CELL_WIDTH "solid" "seagreen")
                                          (* (col-of-cell (car body)) CELL_WIDTH)
                                          (* (quotient (car body) COUNT_CELLS) CELL_WIDTH)
                                          "left" "top" (aux (cdr body) bg (add1 acc)))
                       (place-image/align (square CELL_WIDTH "solid" "green")
                                          (* (col-of-cell (car body)) CELL_WIDTH)
                                          (* (row-of-cell (car body)) CELL_WIDTH)
                                          "left" "top" (aux (cdr body) bg (add1 acc))))]))]
    (aux body0 bg 1)))

;; game -> game
;; create the next game state
(define (next g)
  (cond [(= (game-dir g) RIGHT)(game (cons (add1 (car (game-snake-body g))) 
                                           (take (game-snake-body g)
                                                 (sub1 (length (game-snake-body g))))) (game-dir g) (game-score g))]
        [(= (game-dir g) LEFT) (game (cons (sub1 (car (game-snake-body g))) 
                                           (take (game-snake-body g)
                                                 (sub1 (length (game-snake-body g))))) (game-dir g) (game-score g))]
        [(= (game-dir g) UP) (game (cons (- (car (game-snake-body g)) COUNT_CELLS) 
                                         (take (game-snake-body g)
                                               (sub1 (length (game-snake-body g))))) (game-dir g) (game-score g))]
        [(= (game-dir g) DOWN) (game (cons (+ (car (game-snake-body g)) COUNT_CELLS) 
                                           (take (game-snake-body g)
                                                 (sub1 (length (game-snake-body g))))) (game-dir g) (game-score g))]
        ))

; listof cells -> listof cells
; move the body of the snake to the right direction
(define (move-right cl head)
  (if (= head HEAD_END) (append (cdr cl) (cons (add1 (list-ref cl (sub1 (length cl)))) empty)) (cons (add1 (car cl)) (take cl (sub1 (length cl))))
      ))
;; listof cells, head postion, direction -> boolean
;; check if the direction is valid for the body of the snake
(define (valid-direction? body head dir)
  (cond [(= dir RIGHT) (not (go-left? body head))]
        [(= dir LEFT) (not (go-right? body head))]
        [(= dir UP) (not (go-down? body head))]
        [(= dir DOWN) (not (go-up? body head))]))

;; listof cells, head position -> boolean
;; check if the snake is moving to the left direction
(define (go-left? body0 head)
  (local [(define headpos (if (= head HEAD_START) (car body0) (list-ref body0 (sub1 (length body0)))))
          (define col-of-head (col-of-cell headpos))
          (define headlessbody (remove headpos body0))
          (define (aux body coh)
            (cond [(empty? body) #t]
                  [(>= col-of-head (col-of-cell (car body))) #f]
                  [else (aux (cdr body) head)]
                  ))]
    (aux headlessbody col-of-head)))

;; listof cells, head position -> boolean
;; check if the snake is moving to the right direction
(define (go-right? body0 head)
  (local [(define headpos (if (= head HEAD_START) (car body0) (list-ref body0 (sub1 (length body0)))))
          (define col-of-head (col-of-cell headpos))
          (define headlessbody (remove headpos body0))
          (define (aux body coh)
            (cond [(empty? body) #t]
                  [(<= col-of-head (col-of-cell (car body))) #f]
                  [else (aux (cdr body) head)]
                  ))]
    (aux headlessbody col-of-head)))

;; listof cells, head position -> boolean
;; check if the snake is moving to the down direction
(define (go-down? body0 head)
  (local [(define headpos (if (= head HEAD_START) (car body0) (list-ref body0 (sub1 (length body0)))))
          (define row-of-head (row-of-cell headpos))
          (define headlessbody (remove headpos body0))
          (define (aux body roh)
            (cond [(empty? body) #t]
                  [(<= row-of-head (row-of-cell (car body))) #f]
                  [else (aux (cdr body) head)]
                  ))]
    (aux headlessbody row-of-head)))

;; listof cells, head position -> boolean
;; check if the snake is moving to the up direction
(define (go-up? body0 head)
  (local [(define headpos (if (= head HEAD_START) (car body0) (list-ref body0 (sub1 (length body0)))))
          (define row-of-head (row-of-cell headpos))
          (define headlessbody (remove headpos body0))
          (define (aux body roh)
            (cond [(empty? body) #t]
                  [(>= row-of-head (row-of-cell (car body))) #f]
                  [else (aux (cdr body) head)]
                  ))]
    (aux headlessbody row-of-head)))


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