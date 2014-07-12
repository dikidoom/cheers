#lang racket/base

(require 2htdp/image
         racket/math
         racket/list
         racket/function
         racket/format)

(define (render b)                      ; binge -> image
  (define size 200)
  (define pts (circular-spread (binge-size b)
                               #:width (* size 1/3)
                               #:height (* size 1/3)
                               #:offset-x (/ size 2)
                               #:offset-y (/ size 2)))
  (define base (square size 'solid "white")) ;(empty-scene size size)
  ;; add circles and numbres
  (for ([pt (in-list pts)]
        [i (in-naturals)])
    (set! base
          (place-image (overlay (text (number->string i) 16 "black")
                                (circle 12 'outline "black"))
                       (first pt)
                       (second pt)
                       base)))
  ;; at this point, the base image is clean.
  ;; now we draw each move on a new image.
  (define (line img m color)
    (let ([p1 (list-ref pts (car m))]
          [p2 (list-ref pts (cdr m))])
      (add-line img
                (first p1) (second p1)
                (first p2) (second p2)
                color)))
  (define (lineate img moves)
    (define copy img)
    (for ([ms (in-list moves)]
          [i (in-naturals)]
          #:when #t
          [m (in-list ms)])
      (set! copy
            (line copy m (if (equal? i 0)
                             "red"
                             "gray"))))
    copy)
  (define series
    (for/list ([i (in-range 1
                            (+ 1 (length (binge-moves b))))])
      (define subset (take-right (binge-moves b) i))
      (lineate base subset)))
  (frame (apply above series)))

(define (circular-spread [count 12]
                         #:width [width 1]
                         #:height [height 1]
                         #:offset-x [x 0]
                         #:offset-y [y 0])
  (let ([frac (/ (* 2 pi) count)])
    (for/list ([i count])
      (let ([r (* frac i)])
        (list (+ x (* (sin r) width))
              (+ y (* (cos r) height))
              r)))))


;; -----------------------------------------------------------------------------
(struct binge (size state moves)
        #:transparent)

;; STATE: '(move ...)
;; MOVES: '(move ...)
;; move: (from . to) where (< from to)
;; moves: '(move ...)

(define (new-binge size)
  (binge size '() '()))

(define (apply-move b m)
  (struct-copy binge b
               [state (sort-moves (append m (binge-state b)))]
               [moves (cons m (binge-moves b))]))

(define (next-moves b)
  (define possible-moves '(()))
  (for ([i (in-range (binge-size b))])  ; every member
    (define pinned possible-moves)
    ;;(printf "member ~a, from ~a\n" i pinned)
    (for* ([m (in-list pinned)]    ; every previous move in this round
           [o (in-range (binge-size b))] ; every other member
           #:unless (or (equal? i o)
                        ;; already cheered in binge-state
                        (cheered? b (cons (min i o)
                                          (max i o)))
                        ;; busy (cheering other) this move
                        (or (busy? m o)
                            (busy? m i))
                        ;; crossing this move
                        (crossing? m (cons (min i o)
                                           (max i o)))))
      ;;(printf "for ~a: ~a\n" m (cons i o))
      (set! possible-moves
            (cons (cons (cons (min i o)
                              (max i o))
                        m)
                  possible-moves))))
  ;; remove duplicates, thoroughly
  (filter (negate empty?)
          (remove-duplicates
           (map sort-moves
                possible-moves))))

(define (finished? b)                   ; binge -> < #t || #f >
  (for/and ([i (in-range (binge-size b))])
    (equal? (- (binge-size b) 1)
            (length
             (filter (lambda (m) (or (equal? (car m) i)
                                (equal? (cdr m) i)))
                     (binge-state b))))))

;; TODO remove quasi-duplicates - binges with equal state & move-count, only different move ordering
(define (solve size)
  (define finished '())
  (define (run bs)
    (define next-binges
      (flatten
       (for/list ([b (in-list bs)])
         (map (curry apply-move b)
              (next-moves b)))))    
    (define unique-binges
      (remove-duplicates next-binges
                         #:key (lambda (b) (cons (binge-state b)
                                            (length (binge-moves b))))))
    (define-values (done rest)
      (partition finished? unique-binges))
    (set! finished (append done finished))
    (if (empty? rest)
        finished
        (run rest)))
  (run (list (new-binge size))))

;; -----------------------------------------------------------------------------
(define (sort-moves ms)
  (sort ms < #:key index #:cache-keys? #f))

(define (index m) ; move -> integer
  (+ (* (car m) 10)
     (cdr m)))

(define (busy? ms i) ; moves player -> < #t | #f >
  (list? (member i (flatten ms))))

(define (cheered? b m) ; binge move -> < #t | #f >
  (list? (member m (binge-state b))))

(define (crossing? ms m) ; moves move -> < #t | #f >
  (for/or ([mv (in-list ms)])
    (or (and (inside? m (car mv))
             (outside? m (cdr mv)))
        (and (inside? m (cdr mv))
             (outside? m (car mv))))))

(define (inside? m v) ; move value -> < #t | #f >
  (and (> v (car m))
       (< v (cdr m))))

(define (outside? m v) ; move value -> < #t | #f >
  (or (and (> v (car m)) (> v (cdr m)))
      (and (< v (car m)) (< v (cdr m)))))  

(define (test-crossing)
  (values (crossing? '((0 . 2)) '(1 . 3))
          (crossing? '((1 . 3)) '(0 . 2))
          (crossing? '((0 . 3)) '(1 . 2))))

;; -----------------------------------------------------------------------------
(define foo (apply-move
             (apply-move (new-binge 6) '((0 . 1) (2 . 5)))
             '((0 . 2) (3 . 4))))

(define qux (solve 4))

(define (vis . b)
  (save-image (apply beside (map render b))
              (~a "render-" (current-seconds) ".png")))

;;(save-image (render foo) "render.png")

;; (save-image (apply beside (map render
;;                                (take-right qux 12)))
;;             "render-4x12.png")
