#lang racket/base

(require racket/gui
         "utils.rkt"
         pict
         )

(define screen-width     800)
(define screen-height    600)
(define x-offset         0)
(define y-offset         screen-height)


(define my-frame (new frame%
                      [label "x"]))

(struct point (x y))

(define my-canvas%
  (class canvas%
    
    (inherit get-dc)
    
    (field
     ;display selection box
     [display-select-box #f] 
     [display-select-picture #f] 
     [select-box '()]
     [image-select-box '()]
     
     ;canvas display variables
     [rotation 0]
     [transformation-matrix (vector 1 0 0 1 0 0)]
     [x-scale 1]
     [y-scale 1]

     ;image display variables
     [image-origin-x 0]
     [image-origin-y 0]
     [image-end-x 0]
     [image-end-y 0]
     [selected-img #f]
     [selected-img-width 0]
     [selected-img-height 0]
     
     ;interaction variables
     [init-cursor-x 0]
     [init-cursor-y 0]
     [cursor-x 0]
     [cursor-y 0]
     [scaled-cursor-x 0]
     [scaled-cursor-y 0]
     )
    
    (define-syntax change-cursor
      (lambda (stx)
        (syntax-case stx ()
          [(_ cursor-type)
           #'(send this set-cursor cursor-type)])))
    
    ;; CURSOR TYPES
    (define normal (make-object cursor% 'arrow))
    (define panning (make-object cursor% 'hand))
    (define selecting (make-object cursor% 'cross))
    
    ;; MOUSE SCALING
    ;scale mouse coordinates to display coordinates
    (define (mouse2display-x x)
      (/ (- x x-offset) x-scale))
    (define (mouse2display-y y) 
      (/ (- y y-offset) y-scale))
    
    (define/public (update-canvas)
      (send (get-dc) set-transformation (vector transformation-matrix x-offset y-offset x-scale y-scale rotation))
      (send this refresh-now))
    
    ;; KEY EVENTS
    (define/override (on-char event)
      (let ((key (send event get-key-code)))
        (special-control-key #t)
        (cond [(equal? key 'wheel-up)
               (set! x-scale (+ x-scale 0.1)) 
               (set! y-scale (+ y-scale 0.1))]
              [(equal? key 'escape) 
               (set! display-select-picture #f)
               (set! selected-img #f)]
              [(equal? key 'wheel-down) 
               (when (> (- x-scale 0.1) 0) 
                 (set! x-scale (- x-scale 0.1))
                 (set! y-scale (- y-scale 0.1)))])
        (update-canvas)))
    
    ;; MOUSE EVENTS
    (define/override (on-event event)
      (define drawer (get-dc))
      
      (set! cursor-x (send event get-x))
      (set! cursor-y (send event get-y))
      (set! scaled-cursor-x (mouse2display-x cursor-x))
      (set! scaled-cursor-y (mouse2display-y cursor-y))
      
      (define-syntax-rule (is-key-event? query)
        (send event query))
      (define (is-mouse-event? query)
        (equal? query (send event get-event-type)))
      
      ;key and mouse combinations
      (define click-right  (is-mouse-event? 'right-down))
      (define click-left   (is-mouse-event? 'left-down))
      (define release-left (is-mouse-event? 'left-up))
      (define hold-ctrl    (is-key-event? get-control-down))
      (define caps-on      (is-key-event? get-caps-down))
      (define dragging     (send event dragging?)) ;click and hold
      
      (define start-panning? click-left)
      (define is-panning? (and dragging (number? init-cursor-x) (number? init-cursor-y)))
      (define end-panning? release-left)
      (define start-selecting? (and click-left hold-ctrl))
      (define is-selecting? (and dragging hold-ctrl))
      (define end-selecting? (and release-left hold-ctrl))
      
      (send this refresh-now)
      
      (cond
        (end-selecting?
         (change-cursor normal)
         (set! display-select-box #f)
         (set! display-select-picture #t)
         (when (within-picture? select-box)
           (let* ((lst select-box)
                  (1st (first lst))
                  (2nd (second lst))
                  (3rd (third lst))
                  (4th (fourth lst))
                  (width  (round-to-int (abs (- (first 1st) (first 3rd)))))
                  (height (round-to-int (abs (- (second 1st) (second 3rd))))))
             (when display-select-picture
               (set! selected-img (make-bytes (* 4 width height)))
               (set! selected-img-width width)
               (set! selected-img-height height)
               (time (send bm-dc get-argb-pixels
                           (round-to-int (smallest (list (first 1st) (first 3rd))))
                           (round-to-int (smallest (list (second 1st) (second 3rd))))
                           width
                           height
                           selected-img)))))
         (update-canvas))
        (end-panning?
         ;see conditions for is-panning? to understand setting init-cursor-x and init-cursor-y values
         (set! init-cursor-x #f)
         (set! init-cursor-y #f)
         (set! display-select-box #f)
         ;fix the offset so that the screen stays where it is after panning is finished
         (set! x-offset (vector-ref (send drawer get-transformation) 1))
         (set! y-offset (vector-ref (send drawer get-transformation) 2))
         (change-cursor normal))
        (start-selecting?
         (set! init-cursor-x scaled-cursor-x)
         (set! init-cursor-y scaled-cursor-y)
         (set! display-select-box #t))
        (start-panning?
         (set! init-cursor-x cursor-x)
         (set! init-cursor-y cursor-y))
        (is-selecting?
         (change-cursor selecting)
         (set! select-box (list (list init-cursor-x init-cursor-y)
                                (list scaled-cursor-x init-cursor-y)
                                (list scaled-cursor-x scaled-cursor-y)
                                (list init-cursor-x scaled-cursor-y))))
                                                                        
        (is-panning?
         (let* ((current-x (- cursor-x init-cursor-x))
                (current-y (- cursor-y init-cursor-y)))
           (change-cursor panning)
           (send drawer set-transformation (vector transformation-matrix (+ current-x x-offset) (+ current-y y-offset) x-scale y-scale rotation))
           (send this refresh)))))
    
    (define bm1 (make-monochrome-bitmap 1 1))
    (define bm2 (make-monochrome-bitmap 600 600))
    (send bm1 load-file  "test1.jpg" 'jpeg #f #t)
    (send bm2 load-file  "test.png" 'png #f #t)
    (define bm-dc (new bitmap-dc% [bitmap bm1]))
    (define-values (width height) (send bm-dc get-size))
    (set! image-end-x (+ image-origin-x width))
    (set! image-end-y (+ image-origin-y height))

    (define pict1 (bitmap bm2))

    (define (point-in-img? p)
      (and (> (first p) image-origin-x)
           (< (first p) image-end-x)
           (> (second p) image-origin-y)
           (< (second p) image-end-y)))

    (define (within-picture? points)
      (ormap point-in-img? points))
    
    (define (draw-select-box lst)
      (unless (empty? lst)
        (define 1st (first lst))
        (define 2nd (second lst))
        (define 3rd (third lst))
        (define 4th (fourth lst))
        (send/apply (get-dc) draw-line (append 1st 2nd))
        (send/apply (get-dc) draw-line (append 2nd 3rd))
        (send/apply (get-dc) draw-line (append 3rd 4th))
        (send/apply (get-dc) draw-line (append 4th 1st))))
    
    (define/override (on-paint)
      (define dc (send this get-dc))
      (send dc draw-bitmap bm1 image-origin-x image-origin-y)
      (when display-select-box     (draw-select-box select-box))
      (when display-select-picture (draw-select-box select-box))
      (send dc draw-line -50 -50 -50 50)
      (send dc draw-line -50 50 50 50)
      (send dc draw-line 50 50 50 -50)
      (send dc draw-line 50 -50 -50 -50))
    (super-new)))

(define my-canvas
  (new my-canvas%
       [parent my-frame]
       [min-width screen-width]
       [min-height screen-height]))

(new button%
     [label "Save as model"]
     [parent my-frame]
     [callback (lambda (b e)
                 (when (bytes? (get-field selected-img my-canvas))
                   (define pixels (get-field selected-img my-canvas))
                   (define width (get-field selected-img-width my-canvas))
                   (define height (get-field selected-img-height my-canvas))
                   (define model (make-bitmap width height))
                   (display (bytes->list pixels))
                   (define (color->gray byte-list)
                     (cond ((empty? byte-list) '())
                           (else (match byte-list
                                   [(list a b c d e ...) (append (list a
                                                                       (round-to-int (/ (+ b c d) 3))
                                                                       (round-to-int (/ (+ b c d) 3))
                                                                       (round-to-int (/ (+ b c d) 3)))
                                                                 (color->gray (cddddr byte-list)))]))))
                   (send model set-argb-pixels 0 0 width height (list->bytes (color->gray (bytes->list pixels))))
                   (send model save-file "model1.jpeg" 'jpeg 100)
                   (define selected-img (argb-pixels->pict pixels width))
                   (show-pict selected-img))
                 )])

(define my-pic (make-object bitmap% "test.png" 'png #f #t))

(send my-frame show #t)
(send my-canvas update-canvas)