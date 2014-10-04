(ql:quickload "sdl2")
(ql:quickload "cl-opengl")

(defpackage :g (:use :cl :gl))
(in-package :g)

(defclass state ()
  ((mouse :accessor mouse :initarg :mouse :initform nil)))

#+nil
(defparameter *bla* (sdl2:create-window))
#+nil
(sdl2:destroy-window *bla*)

#+nil
(basic-test)


(defmethod draw ((win sdl2-ffi:sdl-window) (state state))
  (format t "mouse-state ~a~%" (mouse state))
  (clear :color-buffer)
  
  (with-primitive :triangles
    (color 0.0 2.0 0.0)
    (vertex 0.0 1.0)
    (vertex -1.0 -1.0)
    (vertex 1.0 -1.0))
  (defparameter *w* win)
  (with-pushed-matrix
    (when (mouse state)
      (translate -2.5 -2.5 0d0)
      (translate (/ (first (mouse state)) 128)
		(/ (- 512 (second (mouse state))) 128)
		0d0))
   (with-primitive :lines
     (color 1d0 1d0 1d0)
     (vertex .5 0.0) (vertex .5 1.0)
     (vertex 0.0 .5) (vertex 1.0 .5)))
  (flush)
  (sdl2:gl-swap-window win))

(defun basic-test ()
  "The kitchen sink."
  (sdl2:with-init (sdl2-ffi:+sdl-init-timer+ sdl2-ffi:+sdl-init-video+)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)
    
    (let ((win-width 512)
	  (win-height 512))
     (sdl2:with-window (win :flags '(:shown :opengl) :w win-width :h win-height)
       (sdl2:with-gl-context (gl-context win)
	 (progn 
	   (progn 
	     (finish-output)
	     (sdl2:gl-make-current win gl-context)
	     (viewport 0 0 512 512)
	     (matrix-mode :projection)
	     (ortho -2 2 -2 2 -2 2)
	     (matrix-mode :modelview)
	     (load-identity)
	     (clear-color 0.0 0.0 0.0 1.0)
	     (clear :color-buffer))
	   (let ((state (make-instance 'state)))
	     (sdl2:with-event-loop (:method :poll)
	       (:keydown
		(:keysym keysym)
		(let ((scancode (sdl2:scancode-value keysym))
		      (sym (sdl2:sym-value keysym))
		      (mod-value (sdl2:mod-value keysym)))
		  (cond
		    ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
		    ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
		    ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
		  (format t "Key sym: ~a, code: ~a, mod: ~a~%" sym scancode mod-value)))

	       (:keyup
		(:keysym keysym)
		(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
		  (sdl2:push-event :quit)))

	       (:mousemotion
		(:x x :y y :xrel xrel :yrel yrel :state stat)
		(format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
			x xrel y yrel stat)
		(setf state (make-instance 'state :mouse (list x y))))

	       (:idle
		()
		(draw win state))

	       (:quit () t)))))))))


