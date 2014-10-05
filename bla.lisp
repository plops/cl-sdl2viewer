(eval-when (:execute :load-toplevel :compile-toplevel)
  (ql:quickload "sdl2")
  (ql:quickload "cl-opengl"))

(defpackage :g (:use :cl :gl))
(in-package :g)

(defclass state ()
  ((mouse :accessor mouse :initarg :mouse :initform nil)
   (texture :accessor texture :initarg :texture :initform nil)
   (tex-width :accessor tex-width :initarg :tex-width :initform nil)
   (tex-height :accessor tex-height :initarg :tex-height :initform nil)))

(let ((val 0d0))
 (defmethod draw ((win sdl2-ffi:sdl-window) (renderer sdl2-ffi:sdl-renderer) (state state))
   (incf val .01)
   (with-slots (tex-width tex-height texture) state
    (let ((valb (max 0 (min 255 (floor (* 255 .5 (+ 1 (cos val))))))))
      (progn
	(multiple-value-bind (pixels pitch)
	    (sdl2:lock-texture texture)
	  (declare (type (integer 0 64400) pitch))
	  (dotimes (j tex-height)
	    (let ((line (* j pitch)))
	      (declare (type fixnum line))
	      (dotimes (i tex-width)
		;; 0 .. b, 1 .. g, 2 .. r, 3 .. a
		(let ((pos (+ (* 4 i) line)))
		  (setf 
		   (cffi:mem-ref pixels :uint8 (+ 0 pos)) (mod i 255)
		   (cffi:mem-ref pixels :uint8 (+ 1 pos)) (floor (* 127 (+ 1 (sin (* .1 (+ (* .2 valb) i j))))))
		   (cffi:mem-ref pixels :uint8 (+ 2 pos)) valb
		   (cffi:mem-ref pixels :uint8 (+ 3 pos)) 255))))))
	(sdl2:unlock-texture texture)))
    (progn 
      (sdl2-ffi.functions:SDL-RENDER-CLEAR renderer)
      (sdl2-ffi.functions:SDL-RENDER-COPY renderer texture
					  (cffi:null-pointer)
					  (cffi:null-pointer))
      (sdl2-ffi.functions:SDL-RENDER-PRESENT renderer)))
  ))

(defun basic-test ()
  "The kitchen sink."
  (declare (optimize (speed 3)))
  (sdl2:with-init (sdl2-ffi:+sdl-init-timer+ sdl2-ffi:+sdl-init-video+)    
    (let* ((win-width 256)
	   (win-height win-width)
	   (tex-width 256)
	   (tex-height 256))
      (multiple-value-bind (window renderer)
	  (sdl2:create-window-and-renderer win-width win-height '(:resizable :shown :presentvsync))
	
	(let ((texture (sdl2:create-texture renderer :argb8888 :streaming 
					    tex-width tex-height)))
	  (let ((state (make-instance 'state :tex-width tex-width :tex-height tex-height :texture texture)))
	   (sdl2:with-event-loop (:method :poll)
	     (:keyup () (sdl2:push-event :quit))
	     (:mousebuttondown () (sdl2:push-event :quit))
	     (:idle ()
		    (draw window renderer state))
	     (:quit () t))))))))


#+nil
(basic-test)


