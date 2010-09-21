;; condensed description of lisp

;; the goal is to do simple image processing

;; get the following things:
;; sbcl, emacs, slime, paredit
;; without a good editor there is no point writing lisp
;; emacs is the BEST editor for this task

;; don't waste time trying to make a deliverable executable. if you
;; need that you should use some other language. lisp is for
;; prototyping. ideally you will always run your program from within
;; emacs. there are types of lisp programs that you don't want to
;; write in C (I'm just trying to learn those now). then maybe ECL
;; (compiles to C) or ABCL (compiles to JVM) is a solution.

;; I think it is easier to understand someone elses (good written)
;; medium sized lisp program because you can jump around the functions
;; with M-. and M-, get documentation to functions arguments and have
;; document strings describing important functions. However I miss
;; single-step debugging a bit. I hear some compilers have it but sbcl
;; doesn't.

;; calling functions: open in emacs, put cursor on expression, execute
;; C-M-x
(+ 2 3)
(/ 2.0 3.2d0)
(floor 5 2)
(list 1 2)

;; save data in variables, local
(let* ((a 2)
       (b a)
       (c 0))
  (list c (setf c (+ a b)) c))

;; global, convention: use stars around name
(defparameter *blub* 3)
;; the binding is local, useful for rerouting output
(with-open-file (*standard-output* "/dev/shm/test"
				   :if-does-not-exist :create
				   :if-exists :supersede
				   :direction :output)
  (format t "blub~%"))

;; I use defvar for computations that take long time and don't have to
;; be redone when you reaload the file with C-c C-k. too complicated
;; for now, you'll realize its usefulness
(defvar *blub* 4) 


;; super important: arrays
(let ((a (make-array 3)))
  a)

;; I always use them with type, otherwise I get confused. Access first
;; element:
(let ((a (make-array 3 :element-type 'single-float)))
  (aref a 0))

;; write to first element
(let ((a (make-array 3 :element-type 'fixnum)))
  (setf (aref a 0) 1)
  a)

;; the important types are: 
;; double-float
(type-of 1d0)
;; I always use double float! Always write 1.2d0, not 1.0

;;(complex double-float)
(type-of (complex 1d0 2d0))
(type-of (sqrt -1d0))
(exp (* 2 pi (complex 0 1)))

;; fixnum, 3bits (or so) of the 64bit number are used as type
;; information. I use this for loop variables 

;; for images I use
;; (unsigned-byte 8)
;; note how to define a 2d array
(let ((a (make-array (list 3 3) :element-type '(unsigned-byte 8))))
  a)

;; output, there is only one command you need to know:
(format t "~a~%" (list 34 523))
;; the t means it will go to stdout, the ~a formats any argument, it
;; is intelligent enough to print most things. more elaborate control
;; is possible, e.g. padding with zeros for an integer ~3,'0d or
;; number of digits in a float ~2,3f

;; ~% prints a newline

;; you can also print into string (useful for creating filenames of
;; a stack of images)
(format nil "embryo-~3,'0d.pgm" 15)

;; output into file (we already had this above)
(with-open-file (*standard-output* "/dev/shm/test"
				   :if-does-not-exist :create
				   :if-exists :supersede
				   :direction :output)
  (format t "blub~%"))

;; define functions
(defun square (x)
  (* x x))
(square 3)

;; I consider this a masterpiece, near optimum in clarity and speed,
;; possibly all you ever need to store images (read 'man pgm', color
;; is easy, 16bit are mildly annoying):
(defun write-pgm (filename img)
  "Store 2d ub8 image into file."
  (declare (string filename)
	   ((array (unsigned-byte 8) 2) img)
	   (values null &optional))
  (destructuring-bind (h w) (array-dimensions img)
    (with-open-file (s filename
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (format s "P5~%~D ~D~%255~%" w h))
    (with-open-file (s filename 
		       :element-type '(unsigned-byte 8)
		       :direction :output
		       :if-exists :append)
      (write-sequence (sb-ext:array-storage-vector img) s))
    nil))

;; note the documentation string, retrieve it by pressing C-c C-d C-d
;; on the function name write-pgm below

(let ((a (make-array (list 3 3) :element-type '(unsigned-byte 8))))
  (write-pgm "/dev/shm/o.pgm" a))

;; the (declare .. promises the compiler the types of the
;; arguments. If you call the function with, e.g. a float array, the
;; compiler will warn you. This happens to me all the time. You should
;; really declare the argument types to prevent these stupid bugs.

(array-dimensions #2A((1 2) (3 4))) ;; gives the list you would have
				    ;; supplied to make-array

;; Always store images in this order (y x) or volumes (z y x), then
;; everything is fine. E.g. slices of a z-stack are consecuteive
;; regions in memory, same for horizontal lines.

;; all compilers really store a 2d vector as a 1d data, often they
;; have a function that exposes this
(sb-ext:array-storage-vector #2A((1 2) (3 4)))

;; I only use sbcl. If you don't this is code that runs on all common
;; lisps it creates a 1d array that is "displaced" to the old one. I
;; don't use it because its slower and more code.
(let ((a (make-array (list 3 3) :element-type '(unsigned-byte 8))))
 (make-array (array-total-size a)
	     :element-type '(unsigned-byte 8)
	     :displaced-to a))

;; with-open-file normally opens files so you can write out text
;; characters. I don't really understand how that works in
;; detail. From C I know, that a string can't contain a zero byte
;; because that ends the string. In order to output binary data you
;; supply your element-type and you can send the data of a 1d array
;; into the output stream with write-sequence

;; Now we can store images. Reading them is even nicer. You first read
;; the text, you create the array of appropriate size and then you
;; reopen the file in binary, jump to the file-position where the text
;; ended and read the sequence. I won't show that here.



;; now we just need to understand loops and we can do cool stuff
(dotimes (i 10)
  (format t "~d~%" i))


;; note the star in let*, you always need that if variables depend on
;; each other
(let* ((w 320)
       (h 240)
       (a (make-array (list h w)
		      :element-type '(unsigned-byte 8))))
  (dotimes (j h)
    (dotimes (i w)
      (let* ((y (/ (- j (floor h 2)) h))
	     (x (/ (- i (floor i 2)) w))
	     (r (sqrt (+ (* x x) (* y y)))))
	(setf (aref a j i) (floor (* (/ 255 2) (+ 1 (cos (* 100d0 r)))))))))
  (write-pgm ))

;; I think thats it. Other topics of decreasing importance are:

;; 1) how to use asdf for using other lisp libraries, install
;; cl-opengl

;; 2) other parts of the language if, when, loop, cons, car, subseq,
;; assoc, reverse, comments, #+nil

;; 3) MACROS, that is one of lisps biggest strengths: once you
;; mastered the language a little bit (backquote, intern, format with
;; ~A, gensym, loop with collect) you can actually write lisp that
;; writes more lisp for you. Its so nice I give you my very own
;; precious supermacro:

(defmacro do-region ((indices end &optional (start
					     '(0 0 0))) &body body)
  "Write intertwined loops to traverse a vector, an image or a volume."
  (unless (and (= (length indices)
                  (length end)))
    (error "Number of indices and interval-ends are not equal."))
  (labels ((rec (ind end start acc) ;; several loops
             (if (null end)
                 acc
                 (rec (cdr ind) (cdr end) (cdr start)
                      `((loop for ,(car ind) from ,(car start) 
                           below ,(car end) do ,@acc))))))
    (first (rec (reverse indices) ;; first index is outermost loop
                (reverse end)
                (reverse start) body))))


(let* ((h 3) (w 3)
       (a (make-array (list h w) :element-type '(unsigned-byte 8))))
 (do-region ((k j) (h w))
   (setf (aref a k j) k))
 a)

;; go onto the first ( infront of (do-region and press C-c RETURN,
;; this gives you the macro expansion

#+nil
(LOOP FOR K FROM 0 BELOW H
   DO (LOOP FOR J FROM 0 BELOW W
	 DO (SETF (AREF A K J) K)))

;; you don't have to write the several loops to traverse an image and
;; it even works for volumes! and it doesn't make the programm slower
;; because the compiler eventually sees the expanded code that looks
;; like you would have written it anyway.

;; 4) interfacing to c programs sb-alien, or cross platform but slower
;; cffi. learn the one for your lisp first

;; 5) error, conditions, handler-bind; just throw errors when things
;; can go wrong in your code (like a ray doesn't hit a lens). a higher
;; function will end up in the debugger when this error occurs. you
;; then just install a handler that catches the error and does
;; something useful (e.g. stop raytracing this ray and start with a
;; new one)

;; 6) object orientation. i don't know if this is useful. it makes
;; your implementation harder to read. but sometimes 

;; 7) packages and some more of cltl2, sb-threads, write simple
;; webserver

;; <------------ I'm currently here -------------------------------

;; 8) learn artificial intelligence algorithms

;; 9) understand compiler internals so that you can directly generate
;; assembly code for SSE (nokdemus' raylisp has some optimized matrix
;; stuff for raytracers, christophe rhodes has some cool code on dna
;; and string processing) or lately I even stumbled upon some
;; ukrainian guy who wants to compile a subset into cuda (and the ati
;; version of it),

;; 10) look at maxima and axiom. decide which has more readable
;; codebase, dive into it and learn

;; 11) understand metaobject protocol (the next thing after object
;; orientation) I really doubt that this is of any use. If I would
;; learn it maybe 2 people in the world could read my code.