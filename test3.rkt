#lang racket
(require sgl
         sgl/gl
         sgl/gl-vectors
         sgl/bitmap
         slideshow
         racket/gui)

; make a texture!
(define bm (read-bitmap "model1.jpeg"))
(define b (instantiate bitmap% ((send bm get-width) (send bm get-height))))
(define b-dc (new bitmap-dc% [bitmap b]))
(send b-dc set-background (make-object color% "black"))
(send b-dc clear)
(send b-dc set-smoothing 'aligned)
;(send b-dc set-bitmap bm)
(send b-dc draw-bitmap bm 0 0)

#|
(define b-mask (instantiate bitmap% (*size* *size* #t)))
(define b-mask-dc (new bitmap-dc% [bitmap b-mask]))
(send b-mask-dc set-background (instantiate color% ("black")))
(send b-mask-dc clear)
(draw-pict (cc-superimpose (blank *size*) (standard-fish *size* (/ *size* 2) #:color "white")) b-mask-dc 0 0)
(send b set-loaded-mask b-mask)|#

; converts a racket/gui bitmap% into an array of ARGB bytes.
(define (bitmap->argb-bytes bm)
  (let* ([width (send bm get-width)]
         [height (send bm get-height)]
         [mask (send bm get-loaded-mask)]
         [buffer (make-bytes (* width height 4) 255)])
    (send bm get-argb-pixels 0 0 width height buffer #f)
    (when mask
      (send bm get-argb-pixels 0 0 width height buffer #t))
    buffer))

; converts an array of ARGB bytes into an OpenGL vector.
(define (argb-bytes->gl-rgba-vector argb-bytes)
  (let* ([length (bytes-length argb-bytes)]
         [gl-buf (make-gl-ubyte-vector length)])
    (let loop ([i 0])
      (when (< i length)
        (gl-vector-set! gl-buf (+ i 0) (bytes-ref argb-bytes (+ i 1)))
        (gl-vector-set! gl-buf (+ i 1) (bytes-ref argb-bytes (+ i 2)))
        (gl-vector-set! gl-buf (+ i 2) (bytes-ref argb-bytes (+ i 3)))
        (gl-vector-set! gl-buf (+ i 3) (bytes-ref argb-bytes (+ i 0)))
        (loop (+ i 4))))
    gl-buf))

; convert a bitmap straight to an OpenGL texture.
(define (bitmap->gl-tex bm)
  (gl-enable 'texture-2d)
  (let ([width (send bm get-width)]
        [height (send bm get-height)]
        [gl-tex (gl-vector-ref (glGenTextures 1) 0)]
        [gl-tex-bytes (argb-bytes->gl-rgba-vector (bitmap->argb-bytes bm))])
    (glBindTexture GL_TEXTURE_2D gl-tex)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST_MIPMAP_NEAREST)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
    (gluBuild2DMipmaps GL_TEXTURE_2D GL_RGBA width height GL_RGBA GL_UNSIGNED_BYTE gl-tex-bytes)
    gl-tex))

(define *texture* #f)
(define *init?* #f)
(define *pic* (glGenLists 1))
(define (gl-init)
  (displayln "INIT")
  (set! *init?* #t)
  (glEnable  GL_DEPTH_TEST)
  (glDepthFunc GL_LEQUAL)
  (glNewList *pic* GL_COMPILE)
  (glEndList)
  (gl-light-v 'light1 'ambient (vector->gl-float-vector #(0.5 0.5 0.5 0)))
  (gl-light-v 'light1 'diffuse (vector->gl-float-vector #(1 1 1 0)))
  (gl-light-v 'light1 'position (vector->gl-float-vector #(2 2 2 1)))
  (glEnable  GL_LIGHT1)
  (glEnable  GL_LIGHTING)
  (set! *texture* (bitmap->gl-tex b)))

; fix the viewport when it's been resized
(define (gl-resize width height)
  (unless *init?* (gl-init))
  (glViewport 0 0 width height)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (let ([h (/ height width)])
    (glFrustum -1 1 (- h) h 5.0 60.0))
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))

; draw the screen!
(define (gl-draw)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-clear-depth 1)
  
  (gl-load-identity)
  
  (gl-translate 0 0 -15.0)
  
  (gl-rotate (/ (current-process-milliseconds) 30)  1 1 1) 
  
  (glBindTexture GL_TEXTURE_2D *texture*)
  (gl-color 1 1 1 1)
  (glBegin GL_QUADS)
  (gl-normal 0 0 1)
  (gl-tex-coord 0 0) (gl-vertex -1 -1  1)
  (gl-tex-coord 1 0) (gl-vertex  1 -1  1)
  (gl-tex-coord 1 1) (gl-vertex  1  1  1)
  (gl-tex-coord 0 1) (gl-vertex -1  1  1)
  
  (gl-normal 0 0 -1)
  (gl-tex-coord 1 0) (gl-vertex -1 -1 -1)
  (gl-tex-coord 1 1) (gl-vertex -1  1 -1)
  (gl-tex-coord 0 1) (gl-vertex  1  1 -1)
  (gl-tex-coord 0 0) (gl-vertex  1 -1 -1)
  
  (gl-normal 0 1 0)
  (gl-tex-coord 0 1) (gl-vertex -1  1 -1)
  (gl-tex-coord 0 0) (gl-vertex -1  1  1)
  (gl-tex-coord 1 0) (gl-vertex  1  1  1)
  (gl-tex-coord 1 1) (gl-vertex  1  1 -1)
  
  (gl-normal 0 -1 0)
  (gl-tex-coord 1 1) (gl-vertex -1 -1 -1)
  (gl-tex-coord 0 1) (gl-vertex  1 -1 -1)
  (gl-tex-coord 0 0) (gl-vertex  1 -1  1)
  (gl-tex-coord 1 0) (gl-vertex -1 -1  1)
  
  (gl-normal 1 0 0)
  (gl-tex-coord 1 0) (gl-vertex  1 -1 -1)
  (gl-tex-coord 1 1) (gl-vertex  1  1 -1)
  (gl-tex-coord 0 1) (gl-vertex  1  1  1)
  (gl-tex-coord 0 0) (gl-vertex  1 -1  1)
  
  (gl-normal -1 0 0)
  (gl-tex-coord 0 0) (gl-vertex -1 -1 -1)
  (gl-tex-coord 1 0) (gl-vertex -1 -1  1)
  (gl-tex-coord 1 1) (gl-vertex -1  1  1)
  (gl-tex-coord 0 1) (gl-vertex -1  1 -1)

  (gl-end)
  (gl-flush))

(define glcanvas%
  (class canvas% (super-new)
    (inherit refresh with-gl-context swap-gl-buffers get-parent)
    
    (define/override (on-paint)
      (with-gl-context                                                         
       (lambda ()                                                            
         (gl-draw)                                                             
         (swap-gl-buffers)))  
      (when (send (get-parent) is-shown?)
        (refresh)))
    
    (define/override (on-size width height)
      (displayln "RESIZED")
      (with-gl-context
       (lambda ()
         (gl-resize width height)
         (swap-gl-buffers)))
      (refresh))
    ))

(define (run)
  (let* ((frame (new frame% (label "OpenGL Window")))
         (glcanvas (new glcanvas% (parent frame)
                        (min-width 640)
                        (min-height 480)
                        (style '(no-autoclear gl)))))
   (unless (send (send (send glcanvas get-dc) get-gl-context) ok?)
      (displayln "Error: OpenGL context failed to initialize")
      (exit))
    (send frame show #t)))

;bm
;b
(run)