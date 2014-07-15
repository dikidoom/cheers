#lang racket/base

;; cheers/main.rkt
;; 
;; when more than 3 people are having drinks and want to toast each other individually,
;; things get complicated. computers are indispensable in solving this problem.
;;
;; use:
;; (define foo (solve n))  ; where n is number of people, preferably < 9
;; (vis foo)               ; saves an instructional PNG
;; see function signature of solve for more

(provide solve
         vis)

(require 2htdp/image
         racket/math
         racket/list
         racket/function
         racket/format)

;; -----------------------------------------------------------------------------
;; config
(define debug? #t)                      ; turns on logging
;; -----------------------------------------------------------------------------
;; public
(define (solve size                     ; number -> (list-of binge)
               #:remove-dups? [remove-dups? #t] ; performance tweak: reduce binges with same state & move count (very rough!)
               #:fast-moves? [fast-moves? #t] ; performace tweak: only recur on fast moves (most links per move), discard others
               #:fast-finish? [fast-finish? #t] ; performance tweak: return on first binge that links all members
               )
  (define finished '())
  (define (run bs)
    (define next-binges
      (flatten
       (for/list ([b (in-list bs)])
         (define moves
           (let* ([ms (next-moves b)]
                  [ls (sort (map length ms) >)]
                  [c (count (curry equal? (first ls)) ls)])
             (log "spreading ~a moves, longest ~a, reduce to ~a\n" (length ms) (first ls) c)
             (if fast-moves?
                 (take (sort ms > #:key length) c)
                 ms)))
         (map (curry apply-move b) moves)
         ;;(apply-move b (first moves)) ; ridiculous oversight mode (fast :P)
         )))    
    (log "generated ~a new binges\n" (length next-binges))
    (define unique-binges
      ;; TODO maybe implement a more granular, discriminating key?
      (remove-duplicates next-binges
                         #:key (lambda (b) (cons (binge-state b)
                                            (length (binge-moves b))))))
    (log "discarded ~a duplicates\n" (- (length next-binges)
                                        (length unique-binges)))
    (define-values (done rest)
      (partition finished? (if remove-dups?
                               unique-binges
                               next-binges)))
    (log "found ~a finals\nrecur on ~a others\n" (length done) (length rest))
    (set! finished (append done finished))
    (if (if fast-finish?
            (not (empty? finished))
            (empty? rest))
        finished
        (run rest)))
  ;;
  (run (list (new-binge size))))

(define (vis b)                         ; (list-of binge) -> void
  (let ([file-name (~a "render-" (current-seconds) ".png")])
    (if (equal? 1 (length b))
        (save-image (render (first b)) file-name)
        (save-image (apply beside (map render b)) file-name))))

;; -----------------------------------------------------------------------------
;; model
;; STATE: '(move ...)
;; MOVES: '(move ...)
;; move: (from . to) where (< from to)
;; moves: '(move ...)
(struct binge (size state moves)
        #:transparent)

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
    (for* ([m (in-list pinned)]    ; every previous move in this round
           [o (in-range i (binge-size b))] ; every other member
           #:unless (or (equal? i o)
                        ;; already cheered in binge-state?
                        (cheered? b (cons (min i o)
                                          (max i o)))
                        ;; busy (cheering other) this move?
                        (or (busy? m o)
                            (busy? m i))
                        ;; crossing this move?
                        (crossing? m (cons (min i o)
                                           (max i o)))))
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

;; -----------------------------------------------------------------------------
;; helpers (logic)
(define (sort-moves ms)
  (sort ms < #:key index #:cache-keys? #f))

(define (index m)                       ; move -> integer
  (+ (* (car m) 10)
     (cdr m)))

(define (finished? b)                   ; binge -> < #t || #f >
  (for/and ([i (in-range (binge-size b))])
    (equal? (- (binge-size b) 1)
            (length
             (filter (lambda (m) (or (equal? (car m) i)
                                (equal? (cdr m) i)))
                     (binge-state b))))))

(define (busy? ms i)                    ; moves player -> < #t | #f >
  (list? (member i (flatten ms))))

(define (cheered? b m)                  ; binge move -> < #t | #f >
  (list? (member m (binge-state b))))

(define (crossing? ms m)                ; moves move -> < #t | #f >
  (for/or ([mv (in-list ms)])
    (or (and (inside? m (car mv))
             (outside? m (cdr mv)))
        (and (inside? m (cdr mv))
             (outside? m (car mv))))))

(define (inside? m v)                   ; move value -> < #t | #f >
  (and (> v (car m))
       (< v (cdr m))))

(define (outside? m v)                  ; move value -> < #t | #f >
  (or (and (> v (car m)) (> v (cdr m)))
      (and (< v (car m)) (< v (cdr m)))))  

(define (circular-spread [count 12]     ; whatevers -> (list-of (list x y r))
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


(define (log str . args)
  (when debug? (apply printf str args)))

;; -----------------------------------------------------------------------------
;; helpers (render)
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
    (for ([ms (in-list (reverse moves))]
          [i (in-range (length moves) 0 -1)] ; (in-naturals)
          #:when #t
          [m (in-list ms)])
      (set! copy
            (line copy m (if (equal? i 1)
                             (pen "red" 2 'solid 'round 'round);;"red"
                             "gray"))))
    copy)
  (define series
    (for/list ([i (in-range 1
                            (+ 1 (length (binge-moves b))))])
      (define subset (take-right (binge-moves b) i))
      (lineate base subset)))
  (frame (apply above series)))

