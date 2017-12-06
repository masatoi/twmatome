(in-package :matome)

(defmacro defroute (name (params &rest route-args) &body body)
  `(setf (ningle:route *app* ,name ,@route-args)
	 (lambda (,params) ,@body)))

(defparameter *app* (make-instance 'ningle:<app>))
(defparameter *handler* nil)

(defparameter *app-root-directory* (asdf:system-source-directory :twmatome))
(defparameter *static-directory*   (merge-pathnames #P"static/" *app-root-directory*))

(defun start (&key (port 5000))
  (setf *handler*
	(clack:clackup
         (lack:builder
	  (:static :path (lambda (path)
			   (if (ppcre:scan "^(?:/images/|/css/|/js/|/fonts/|/robot\\.txt$|/favicon.ico$)" path)
                               path
                               nil))
		   :root *static-directory*)
	  *app*)
         :server :woo
         :use-default-middlewares nil
         :port port)))

(defun stop () (clack:stop *handler*))

(defroute "/" (params)
  (cl-markup:html5
   (:body
    (:p "Hello, world!")
    (:img :src "/images/renzuru-symbol-twitter-icon.png")
    (:p (format nil "params: ~A" params))
    )))

(defun publish-collection (col &key (style "twitter-timeline"))
  (assert (or (string= style "twitter-timeline")
              (string= style "twitter-grid")))
  (cl-markup:markup
   (:a :class style
       :href (concatenate 'string (url col) "?ref_src=twsrc%5Etfw")
       (name col))
   (:|script async|
    :src "https://platform.twitter.com/widgets.js"
    :charset "utf-8")))
