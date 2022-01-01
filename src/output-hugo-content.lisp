(in-package :matome)

(defun publish-collection (col &key (style "twitter-timeline"))
  (assert (or (string= style "twitter-timeline")
              (string= style "twitter-grid")))
  (cl-markup:markup
   (:div :class "timeline-wrapper"
         (:a :class style
             :href (concatenate 'string (url col) "?ref_src=twsrc%5Etfw")
             (name col)))
   (:|script async|
    :src "https://platform.twitter.com/widgets.js"
    :charset "utf-8")))

(defun make-hugo-content (col &optional (dest t))
  (let ((timestr (local-time:format-timestring nil (local-time:now))))
    (format dest
            "+++~%Categories = []~%Description = \"\"~%Tags = [\"fgo\"]~%date = \"~A\"~%title = \"~A\"~%+++~%~A~%"
            timestr
            (name col)
            (publish-collection col))))
