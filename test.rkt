#lang racket/gui 
 
(require (lib "gl.ss" "sgl") 
         (lib "gl-vectors.ss" "sgl")
         sgl/bitmap
         ) 
 
 
(define bm (read-bitmap "model1.jpeg"))

(define (resize w h) 
  (glViewport 0 0 w h) 
  #t)
 
(define (draw-opengl) 
  (glClearColor 0.0 0.0 0.0 0.0) 
  (glClear GL_COLOR_BUFFER_BIT) 
  (glColor3d 1.0 1.0 1.0)

  #|
   (let* ([config (new gl-config%)]
          [text-target (make-gl-bitmap 11 11 config)]
          [bm-dc (new bitmap-dc% [bitmap text-target])])
     (send bm-dc draw-line -100 -100 100 100)
     (send bm-dc draw-line -100 100 100 -100)
     (display (send bm-dc get-gl-context))
     (glCallList (bitmap->gl-list text-target)))
  |#

  
  (glMatrixMode GL_PROJECTION) 
  (glLoadIdentity) 
  (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0) 
  (glMatrixMode GL_MODELVIEW) 
  (glLoadIdentity)
  (glBegin GL_QUADS) 
  (glVertex3d 0.25 0.25 0.0) 
  (glVertex3d 0.75 0.25 0.0) 
  (glVertex3d 0.75 0.75 0.0) 
  (glVertex3d 0.25 0.75 0.0)
  
  (glEnd))
 
 
(define my-canvas% 
  (class* canvas% () 
    (inherit with-gl-context swap-gl-buffers) 
    
    (define/override (on-paint) 
      (with-gl-context 
       (lambda () 
         (draw-opengl) 
         (swap-gl-buffers))))
    
    (define/override (on-size width height) 
      (with-gl-context 
       (lambda () 
         (resize width height))))
    
    (super-instantiate () (style '(gl)))))

(define win (new frame% (label "OpenGl Test") (min-width 200) (min-height 200))) 
(define gl  (new my-canvas% (parent win))) 

(send win show #t) 