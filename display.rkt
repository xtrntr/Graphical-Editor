#lang racket/base

(require racket/gui
         "utils.rkt"
         pict
         )

(define canvas-width     800)
(define canvas-height    600)


(define my-frame (new frame%
                      [label "x"]))

(define my-canvas%
  (class canvas%
    
    (inherit get-dc)
    
    (field
     ;display selection box
     [display-select-box #f] 
     [display-select-picture #f] 
     [select-box '()]
     [image-select-box '()]
     [canvas-width 800]
     [canvas-height 600]
     [x-offset 0]
     [y-offset canvas-height]
     
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
    
    (define (mouse2pixel-x x)
      (let* ([pixel-scale (/ 1 x-scale)]
             [offset-in-pixels (* x-offset pixel-scale)]
             [cursor-pos-in-pixels (* (- cursor-x x-offset) pixel-scale)])
        cursor-pos-in-pixels))
    (define (mouse2pixel-y y)
      (let* ([pixel-scale (/ 1 y-scale)]
             [offset-in-pixels (* y-offset pixel-scale)]
             [cursor-pos-in-pixels (* (- cursor-y y-offset) pixel-scale)])
        cursor-pos-in-pixels))

    ;; 0,0 starts at the top corner, and x and y are positive rightwards and downwards respectively.
    (define/public (starting-coordinates)
      (let* ([pixel-scale (/ 1 x-scale)]
             [left-edge-x (* -1 x-offset pixel-scale)]
             [top-edge-y (* -1 y-offset pixel-scale)])
        (values left-edge-x top-edge-y)))
    
    ;; what this really does is adjust the new x-offset
    (define (center-on)
      (let* ([pixel-cursor-x (mouse2pixel-x cursor-x)]
             [pixel-cursor-y (mouse2pixel-y cursor-y)]
             ;we want to keep the pixel-cursor-x and pixel-cursor-y at cursor-x and cursor-y
             ;after we zoom out
             [pixel-scale (/ 1 (+ x-scale 0.1))]
             ;find out how much cursor-x and cursor-y deviates from the left and top edges in pixels.
             [pixel-x-to-adjust (* cursor-x pixel-scale)]
             [pixel-y-to-adjust (* cursor-y pixel-scale)]
             ;the new left and top edges 
             [pixel-new-left-edge (- pixel-cursor-x pixel-x-to-adjust)]
             [pixel-new-top-edge (- pixel-cursor-y pixel-y-to-adjust)]
             [absolute-new-left-edge (* -1 (/ pixel-new-left-edge pixel-scale))]
             [absolute-new-top-edge (* -1 (/ pixel-new-top-edge pixel-scale))])
        (set! x-scale (+ x-scale 0.1)) 
        (set! y-scale (+ y-scale 0.1))
        (set! x-offset absolute-new-left-edge)
        (set! y-offset absolute-new-top-edge)
        ))

    (define (center-out)
      (let* ([pixel-cursor-x (mouse2pixel-x cursor-x)]
             [pixel-cursor-y (mouse2pixel-y cursor-y)]
             ;we want to keep the pixel-cursor-x and pixel-cursor-y at cursor-x and cursor-y
             [pixel-scale (/ 1 (- x-scale 0.1))]
             ;find out how much cursor-x and cursor-y deviates from the left and top edges in pixels.
             [pixel-x-to-adjust (* cursor-x pixel-scale)]
             [pixel-y-to-adjust (* cursor-y pixel-scale)]
             ;the new left and top edges 
             [pixel-new-left-edge (- pixel-cursor-x pixel-x-to-adjust)]
             [pixel-new-top-edge (- pixel-cursor-y pixel-y-to-adjust)]
             [absolute-new-left-edge (* -1 (/ pixel-new-left-edge pixel-scale))]
             [absolute-new-top-edge (* -1 (/ pixel-new-top-edge pixel-scale))])
        (set! x-scale (- x-scale 0.1))
        (set! y-scale (- y-scale 0.1))
        (set! x-offset absolute-new-left-edge)
        (set! y-offset absolute-new-top-edge)
        ))
    
    (define/public (update-canvas) 
      (send (get-dc) set-transformation (vector transformation-matrix x-offset y-offset x-scale y-scale rotation))
      (send this refresh-now))
    
    ;; KEY EVENTS
    (define/override (on-char event)
      (let ((key (send event get-key-code)))
        (special-control-key #t)
        (cond [(equal? key 'wheel-up)
               (unless (= cursor-x 0) (center-on)
                 )]
              [(equal? key 'escape) 
               (set! display-select-picture #f)
               (set! selected-img #f)]
              [(equal? key 'wheel-down)
               (when (and (not (= cursor-x 0)) (> (- x-scale 0.1) 0))
                 (center-out)
                 )])
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
        (click-right
         (display (format "Cursor X in absolutes: ~a"  cursor-x)) (newline)
         (display (format "Cursor X in pixels: ~a"  (mouse2pixel-x cursor-x))) (newline)
         (display (format "-----------------------")) (newline)
         ;(display (format "X Offset: ~a"  x-offset)) (newline)
         ;(display (format "X Scale: ~a"  x-scale)) (newline)
         ;(display (format "Pixel Scale: ~a"  (/ 1 x-scale))) (newline)
         ;(display (format "Cursor X is ~a pixels away from the left frame"
         ;                 (- (mouse2pixel-x cursor-x) (match/values (send my-canvas starting-coordinates)
         ;                                               [(a _) a])))) (newline)
         ;(display (format "Screen X,Y values: ~a ~a" (send this get-width) (send this get-height)))(newline)
         )
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
    
    (define bm1 (make-monochrome-bitmap 600 600))
    (send bm1 load-file  "39-megapixels.jpg" 'jpeg #f #t)
    (define bm-dc (new bitmap-dc% [bitmap bm1]))
    (define-values (width height) (send bm-dc get-size))
    (set! image-end-x (+ image-origin-x width))
    (set! image-end-y (+ image-origin-y height))

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
       [min-width canvas-width]
       [min-height canvas-height]))

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

(send my-frame show #t)
(send my-canvas update-canvas)