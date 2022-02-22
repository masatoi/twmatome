(defpackage :twmatome
  (:use :cl :chirp :cl-markup :cl-heap :anaphora)
  (:nicknames :matome)
  (:shadowing-import-from :cl-markup :html)
  (:export :search-fgo-and-make-col))

(in-package :matome)

;; load a setting file
(handler-case
    (load "~/.chirp")
  (error (e)
    (format t "~A~%" e)))

(defmacro with-retry ((&key (interval 60) (max-retry 10)) &body body)
  (let ((finish? (gensym))
        (result (gensym))
        (cnt (gensym)))
    `(let (,finish? ,result)
       (loop for ,cnt from 1 to ,max-retry
             until ,finish?
             do (handler-case
                    (setf ,result (progn ,@body)
                          ,finish? t)
                  (error (e)
                    (format t "Error: ~A~%" e)
                    (format t "Sleeping while ~A seconds...~%" ,interval)
                    (sleep ,interval)
                    (format t "Restarting (~A/~A)~%" ,cnt ,max-retry))))
       ,result)))

(defun try-search-with-restart (query &key (interval 60) (max-retry 10) (count 100) max-id)
  (with-retry (:interval interval :max-retry max-retry)
    (search/tweets query :count count :max-id max-id)))

(defun cascade-search (query &key (count 100) (interval 5) (max-cycle 100))
  (let ((search-result (try-search-with-restart query :count count))
        (product nil))    
    (loop while search-result
          for cycle from 1 to max-cycle
          do
             (format t "cycle: ~A product-length: ~A~%" cycle (length product))
             (setf product (nconc product search-result)
                   search-result
                   (try-search-with-restart query
                                            :count count
                                            :max-id (1- (id (car (last search-result))))))
             (sleep interval))
    product))

(defun filter-by-followers-count (tweets &key (threshold 1000))
  (remove-if-not
   (lambda (tweet)
     (let ((followers-count (cdr (assoc :followers (counts (user tweet))))))
       (>= followers-count threshold)))
   tweets))

(defun sort-by-retweet-count (tweets)
  (sort (copy-seq tweets)
        (lambda (tw1 tw2)
          (> (cdr (assoc :retweets (counts tw1)))
             (cdr (assoc :retweets (counts tw2)))))))

;; (defparameter *search-result* (cascade-search "#FGO -filter:retweets"))

;; (defparameter *sorted-result*
;;   (sort (copy-seq *search-result*)
;;         (lambda (tw1 tw2)
;;           (> (cdr (assoc :retweets (counts tw1)))
;;              (cdr (assoc :retweets (counts tw2)))))))

;; (defparameter *col* (collections/create "2017/11/16 #FGO"))

;; (dolist (tw (reverse (subseq *sorted-result* 0 100)))
;;   (collections/add *col* (id tw))
;;   (sleep 1))

;; (url *col*) ; => "https://twitter.com/masatoi0/timelines/929055107580248064"
;; "https://twitter.com/masatoi0/timelines/931069748724379649"

(defun search-and-make-col (query)
  (let* ((search-result (cascade-search (format nil "~A -filter:retweets" query)))
         ;; sort by retweets count
         (sorted-result (sort-by-retweet-count search-result))
         (date-str (local-time:format-timestring
                    nil
                    (local-time:now)
                    :format '(:year "/" :month "/" :day)))
         ;; create collection
         (col (with-retry () (collections/create (format nil "~A ~A" date-str query)))))
    ;; add tweets
    (format t "Adding tweets to collection ~A~%" col)
    (dolist (tw (reverse (subseq sorted-result 0 100)))
      (with-retry ()
        (collections/add col (id tw)))
      (sleep 1))
    col))

(defun search-fgo-and-make-col ()
  (search-and-make-col ("#FGO")))

(defparameter *tweet-q* (make-instance 'priority-queue))
(defparameter *last-tweet* nil)

(defun enqueue-fixed-length (queue item priority max-len)
  (if (< (queue-size queue) max-len)
      (enqueue queue item priority)
      (prog1
        (enqueue queue item priority)
        (dequeue queue))))

(defun set-statuses (search-result queue max-len)
  (dolist (tw search-result)
    (enqueue-fixed-length queue tw (cdr (assoc :retweets (counts tw))) max-len)))

(defun make-tweet-q (query &key (count 100) (interval 5) (max-cycle 100) (max-q-len 200))
  (let ((search-result (search/tweets query :count count))
        (tweet-q (make-instance 'priority-queue))
        (last-tweet nil))
    (set-statuses search-result tweet-q max-q-len)
    (setf last-tweet (car (last search-result)))
    (loop while search-result
          for cycle from 1 to max-cycle
          do
             (format t "cycle: ~A~%" cycle)
             (handler-case
                 (progn
                   (setf search-result (search/tweets query
                                                      :count count
                                                      :max-id (1- (id (car (last search-result))))))
                   (set-statuses search-result tweet-q max-q-len)
                   (setf last-tweet (car (last search-result))))
               (error (e)
                 (print e)
                 (sleep 60)))
             (sleep interval))
    (values tweet-q last-tweet)))

(defun tweet-q->list! (tweet-q)
  (let ((result nil))
    (loop (aif (dequeue tweet-q)
               (push it result)
               (return result)))))

;; (defparameter *tweet-q* (make-tweet-q "#FGO OR #FateGO -filter:retweets"))
;; (defparameter *result* (tweet-q->list! *tweet-q*))
    
;; (mapcar (lambda (tw) (cdr (assoc :retweets (counts tw)))) *result*)
;; ;; (12127 5987 2017 1946 1565 1041 724 602 541 541 461 388 357 351 309 200 189 175
;; ;;  168 107 99 98 96 89 84 71 70 66 63 60 58 57 53 52 52 45 43 43 42 42 41 40 38
;; ;;  36 35 35 34 34 32 32 31 30 30 30 26 26 25 24 23 22 20 18 17 17 16 16 15 14 14
;; ;;  13 13 13 13 13 13 13 12 12 11 11 11 11 10 10 10 10 10 10 10 9 9 9 9 9 8 8 8 8
;; ;;  8 8 8 8 8 8 7 7 7 7 7 7 7 7 7 7 7 7 6 6 6 6 6 6 6 6 6 6 6 6 5 5 5 5 5 5 5 5 5
;; ;;  5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
;; ;;  4 4 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)

;; (defparameter col1 (collections/create "2017/11/11 #FGO result-type:popular" :timeline-order "tweet_reverse_chron"))
;; ;; (collections/destroy (id col1))

;; (url col1)

;; (collections/curate col1 (mapcar (lambda (tw) (list :add (id tw))) *result*))


;; (defparameter fgo-search (search-all-time "#FGO -filter:retweets" :interval 10 :max-cycle 20))

;; (defparameter lispmeetup-search (search-all-time "#lispmeetup -filter:retweets"))

;; (defparameter fgo-search-popular
;;   (remove-if-not (lambda (tw)
;;                    (> (cdr (assoc :favorites (counts tw))) 100))
;;                  fgo-search))

;; (mapcar (compose #'id #'user) fgo-search-popular)

(defvar *follow-users-rest*)

(defun follow-users (user-id-list &key (interval 5))
  (if (null user-id-list)
      (progn
        (setf *follow-users-rest* nil)
        'done)
      (handler-case
          (friendships/create :user-id (car user-id-list))
        (error (e)
          (print e)
          (setf *follow-users-rest* user-id-list)
          user-id-list)
        (:no-error (user)
          (format t "followed: ~A~%" user)
          (sleep interval)
          (follow-users (cdr user-id-list) :interval interval)))))


;; (defun generte-page (collection)
;;   (html5
;;    (:head
;;     (:title "hoge-title")
;;     (:meta :charset "utf-8")
;;     (:link :rel "stylesheet" :href "./css/style.css"))
;;    (:body
;;     (:p "hoge")
;;     (:br))))

