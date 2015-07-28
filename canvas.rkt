#lang racket/gui 
 
(require sgl/bitmap
         sgl/gl
         sgl/init
         (prefix-in gl- sgl/sgl)
         racket/draw
         )

(define bm (read-bitmap "model1.jpeg"))
 
(define (resize w h) 
  (glViewport 0 0 w h)
  #t)
 
(define (draw-opengl)
  (let* ([text-target (make-bitmap (send bm get-width) (send bm get-height))]
         [dc (new bitmap-dc% [bitmap text-target])])
    (send dc set-bitmap bm)
    (glCallList (bitmap->gl-list text-target)))
  (glEnd)
  (glFlush))
 
(define my-canvas% 
  (class* canvas% () 
    (inherit refresh with-gl-context swap-gl-buffers get-parent get-width get-height) 
     
   (define/override (on-paint) 
      (with-gl-context (lambda () 
                         (draw-opengl) 
                         (swap-gl-buffers))))

    (define (common-setup)
      (gl-viewport 0 0 (get-width) (get-height))
      (gl-matrix-mode 'projection)
      (gl-load-identity)
      (gl-ortho -1 1 -1 1 -1 1)
      
      (gl-matrix-mode 'modelview)
      (gl-load-identity)
      ;; Turn off depth test = 2D drawing
      (gl-disable 'depth-test)
      ;; Don't cull anything, so we can flip around the y axis and still render
      (gl-disable 'cull-face)
      
      ;; Turn up quality
      (gl-enable 'blend)
      (gl-blend-func 'src-alpha 'one-minus-src-alpha)
      (gl-enable 'point-smooth)
      (gl-enable 'line-smooth)
      (gl-enable 'polygon-smooth)
      (gl-hint 'point-smooth-hint 'nicest)
      (gl-hint 'line-smooth-hint 'nicest)
      (gl-hint 'polygon-smooth-hint 'nicest)
      (gl-enable 'multisample)
      
      ;; Clear everything to white background
      ;; Comment this if you want to see funky junk 
      ;(gl-clear-color 1. 1. 1. 1.)
      ;(gl-clear 'color-buffer-bit)
      
      ;; Default color is black
      ;(gl-color 0. 0. 0. .9)
      )
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (resize width height)
         (common-setup)
         (refresh))))
     
    (super-instantiate ()
      (style '(gl no-autoclear))
      (gl-config
       (let ([c (new gl-config%)])
         (send c set-multisample-size 4)
         c)))))
 
(define win (new frame% (label "OpenGl Test") (min-width 500) (min-height 500))) 
(define gl  (new my-canvas% (parent win))) 
 
(send win show #t) 