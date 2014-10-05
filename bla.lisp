(eval-when (:execute :load-toplevel :compile-toplevel)
  (ql:quickload "sdl2")
  (ql:quickload "cl-opengl"))

(defpackage :g (:use :cl :gl))
(in-package :g)

(defclass state ()
  ((mouse :accessor mouse :initarg :mouse :initform nil)))

#+nil
(defparameter *bla* (sdl2:create-window))
#+nil
(sdl2:destroy-window *bla*)

(defparameter *surf* nil)

(defmethod draw ((win sdl2-ffi:sdl-window) (state state))
  #+nil
  (unless *surf*
    (setf *surf* (sdl2:create-texture sdl2-ffi::+sdl-textureaccess-streaming+)))
  (format t "mouse-state ~a~%" (mouse state))
  (clear :color-buffer)
  
  (with-pushed-matrix
    (translate 100 100 0)
    (with-primitive :triangles
      
      (color 0.0 1.0 .2)
      (vertex 0.0 31.0)
      (vertex -31.0 -31.0)
      (vertex 31.0 -31.0)))
  
  (multiple-value-bind (win-width win-height)
      (sdl2:get-window-size win)
    (with-pushed-matrix
      (when (mouse state)
	(translate (first (mouse state))
		   (- win-height (second (mouse state)))
		   0d0))
      (line-width 2)
      (with-primitive :lines
	(color 1d0 1d0 1d0)
	(vertex 0 -100) (vertex 0 100)
	(vertex -50 0) (vertex 50 0))
      (line-width 1)))
  
  (flush)
  (sdl2:gl-swap-window win))

(defun basic-test ()
  "The kitchen sink."
  (declare (optimize (debug 3)))
  (sdl2:with-init (sdl2-ffi:+sdl-init-timer+ sdl2-ffi:+sdl-init-video+)    
    (let* ((win-width 512)
	   (win-height win-width)
	   (tex-width win-width)
	   (tex-height win-height))
      (multiple-value-bind (window renderer)
	  (sdl2:create-window-and-renderer win-width win-height '(:resizable :shown))
	
	(let ((texture (sdl2:create-texture 
			renderer 
			:argb8888
			:streaming 
			tex-width tex-height)))
	  (sdl2:with-event-loop (:method :poll)
	    (:keyup () (sdl2:push-event :quit))
	    (:mousebuttondown () (sdl2:push-event :quit))
	    (:idle ()
		   #+Nil (progn
		     (multiple-value-bind (pixels pitch)
			 (sdl2:lock-texture texture)
		       (when (and pixels pitch)
			 (format t "~a~%" (list pixels pitch))
			 (dotimes (i tex-width)
			 (dotimes (j tex-height)
			   (setf (cffi:mem-ref pixels :uint32
					       (+ i (* j pitch)))
				 (mod i 200))))))
		    (sdl2:unlock-texture texture))
		   (progn 
		     (sdl2-ffi.functions:SDL-RENDER-CLEAR renderer)
		     ;; (sdl2-ffi.functions:SDL-RENDER-COPY renderer texture 0 0)
		     (sdl2-ffi.functions:SDL-RENDER-PRESENT renderer)))
	   (:quit () t)))))))


#+nil
(basic-test)


#+nil
(sdl2:with-gl-context (gl-context win)
	 (progn 
	   (progn 
	     (finish-output)
	     (sdl2:gl-make-current win gl-context)
	     (viewport 0 0 win-width win-height)
	     (matrix-mode :projection)
	     (ortho 0 win-width 0 win-height -2 2)
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

	       (:quit () t)))))
