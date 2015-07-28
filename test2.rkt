#lang racket

(require (lib "gl.ss" "sgl")
         (lib "gl-vectors.ss" "sgl")
         sgl/bitmap
         (lib "class.ss")
         (lib "mred.ss" "mred")
         )

(define bm (read-bitmap "model1.jpeg"))
(define (sx-bitmap-new)
  ;(make-object bitmap% width height #f)
  (let ((config (new gl-config%)))
    (let* ((b  (instantiate bitmap% ((send bm get-width) (send bm get-height))))
           (dc (new bitmap-dc% (bitmap b)))
           (gl (send dc get-gl-context)))
      (send dc set-background (make-object color% "black"))
      (send dc clear)
      (send dc set-smoothing 'aligned)
      (send dc draw-bitmap bm 0 0)
      ;(display "gl: ")(display gl)(newline)
      b)))
 
(define (resize w h)
  ;(display "resize: ")
  (glViewport 0 0 w h)
  #t)

(define (draw-opengl)
  (display "draw: ")
  (glClearColor 1 1 1 1)
  (glClear GL_COLOR_BUFFER_BIT)
  (glColor3d 0 0 0)
  (let ([t (current-milliseconds)])
    (glMatrixMode GL_PROJECTION) ;(gl-matrix-mode 'modelview)
    (glLoadIdentity)
    (glScaled 0.5 0.5 0.5) ;(gl-scale 0.5 0.5 0.5)
    ;(gl-translate 0.5 0.5 0)
    (glBegin GL_QUADS)
    (glCallList (bitmap->gl-list (sx-bitmap-new)))
    (glEnd)))

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

(sx-bitmap-new)
(send win show #t)