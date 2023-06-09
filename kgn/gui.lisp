;;----------------------------------------------------------------------------
;; To try it, compile and load this file and then execute:
;;
;;      (kgn::kgn)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 2020 Mark Watson. All rights reserved.
;;----------------------------------------------------------------------------

(in-package #:kgn)

(defvar *width* 1370)
(defvar *best-width* 1020)
(defvar *show-info-pane* t)

(defvar *pane2-message*
  "In order to process your query a series of SPARQL queries will be formed based on the query. These generated SPARQL queries will be shown here and the reuslts of the queries will be formatted and displayed in the results display pane below.")

(defvar *pane3-message*
  "Enter a query containing entities like people's names, companys, places, etc. following by the RETURN key to start processing your query. You can also directly use a DBPedia URI for an entity, for example: <http://dbpedia.org/resource/Apple_Inc.> When you start this application, a sample query is randomly chosen to get you started.")

(defun test-callback-click (selected-node-name)
  (ignore-errors
    (format nil "* user clicked on node: ~A~%" selected-node-name)))

(defun test-callback-click-shift (selected-node-name)
  (ignore-errors
    (if (equal (subseq selected-node-name 0 5) "<http")
        (trivial-open-browser:open-browser 
         (subseq selected-node-name 1 (- (length selected-node-name) 1))))
    (format nil "* user shift-clicked on node: ~A - OPEN WEB BROWSER~%" selected-node-name)))

(defun cache-callback (&rest x) (declare (ignore x))
  (if *USE-CACHING*
      (capi:display (make-instance 'options-panel-interface))))

(defun website-callback (&rest x) (declare (ignore x)) (trivial-open-browser:open-browser "http://www.knowledgegraphnavigator.com/"))

(defun toggle-grapher-visibility (&rest x)
  (declare (ignore x))
  (setf *show-info-pane* (not *show-info-pane*)))

(defvar *examples*)
(setf *examples* '("Bill Gates and Melinda Gates at Microsoft in Seattle"
                   "Bill Clinton <http://dbpedia.org/resource/Georgia_(U.S._state)>"
                   "Bill Gates and Steve Jobs visited IBM and Microsoft in Berlin, San Francisco, Toronto, Canada"
                   "Steve Jobs lived near San Francisco and was a founder of <http://dbpedia.org/resource/Apple_Inc.>"
                   "<http://dbpedia.org/resource/Bill_Gates> visited IBM"
                   "<http://dbpedia.org/resource/Bill_Gates> visited <http://dbpedia.org/resource/Apple_Inc.>"
                   "Bill Gates visited <http://dbpedia.org/resource/Apple_Inc.>"))

(capi:define-interface kgn-interface ()
  ()
  (:menus
   (action-menu 
    "Actions"
    (
     ("Copy generated SPARQL to clipboard"
      :callback
      #'(lambda (&rest x) (declare (ignore x))
          (let ((messages (capi:editor-pane-text text-pane2)))
            (capi::set-clipboard text-pane2 (format nil "---- Generated SPARQL and comments:~%~%~A~%~%" messages) nil))))
     ("Copy results to clipboard"
      :callback
      #'(lambda (&rest x) (declare (ignore x))
          (let ((results (capi:editor-pane-text text-pane3)))
            (capi::set-clipboard text-pane2 (format nil "---- Results:~%~%~A~%" results) nil))))
     ("Copy generated SPARQL and results to clipboard"
      :callback
      #'(lambda (&rest x) (declare (ignore x))
          (let ((messages (capi:editor-pane-text text-pane2))
                (results (capi:editor-pane-text text-pane3)))
            (capi::set-clipboard
             text-pane2
             (format nil "---- Generated SPARQL and comments:~%~%~A~%~%---- Results:~%~%~A~%" messages results) nil))))
     ("Visit Knowledge Graph Navigator Web Site" :callback 'website-callback)
     ("Clear query cache" :callback 'cache-callback)
     ((if *show-info-pane*
          "Stop showing Grapher window for new results"
        "Start showing Grapher window for new results")
      :callback 'toggle-grapher-visibility)
     )))
  (:menu-bar action-menu)
  (:panes
   (text-pane1
    capi:text-input-pane
    :text (nth (random (length *examples*)) *examples*)
    :title "Query"
    :min-height 80
    :max-height 100
    :max-width *width*
    ;;:min-width (- *width* 480)
    :width *best-width*
    :callback 'start-progress-bar-test-from-background-thread)

   (progress-bar
    capi:progress-bar
    :start 0
    :end 100
    )

   (text-pane2
    capi:collector-pane
    :font "Courier"
    :min-height 210
    :max-height 250
    :title "Generated SPARQL queries to get results"
    :text "Note: to answer queries, this app makes multipe SPARQL queries to DBPedia. These SPARQL queries will be shown here."
    :vertical-scroll t
    :create-callback #'(lambda (&rest x)
                         (declare (ignore x))
                         (setf (capi:editor-pane-text text-pane2) *pane2-message*))
    :max-width *width*
    :width *best-width*
    :horizontal-scroll t)

   (text-pane3
    capi:collector-pane ;; capi:display-pane ;; capi:text-input-pane
    :text *pane3-message*
    :font "Courier"
    :line-wrap-marker nil
    :wrap-style :split-on-space
    :vertical-scroll :with-bar
    :title "Results"
    :horizontal-scroll t
    :min-height 220
    :width *best-width*
    :create-callback #'(lambda (&rest x)
                         (declare (ignore x))
                         (setf (capi:editor-pane-text text-pane3) *pane3-message*))
    :max-height 240
    :max-width *width*)
   (info
    capi:title-pane
    :text "Use natural language queries to generate SPARQL"))
  (:layouts
   (main-layout
    capi:grid-layout
    '(nil info
      nil text-pane1
      nil text-pane2
      nil text-pane3
      nil progress-bar)
     :x-ratios '(1 99)
    :has-title-column-p t))
  (:default-initargs
   :layout 'main-layout
   :title "Knowledge Graph Navigator"
   :best-width *best-width*
   :max-width *width*))

(defun start-progress-bar-test-from-background-thread (query-text self)
  (format t "~%** ** entering start-progress-bar-test-from-background-thread:~%~%self=~S~%~%" self)
  (with-slots (text-pane2 text-pane3  progress-bar) self
    (print text-pane2)
    (flet ((update-progress-bar (percent)
             (capi:execute-with-interface
              self
              #'(lambda ()
                  (setf (capi:range-slug-start progress-bar) percent)))))
      (mp:process-run-function "progress-bar-test-from-background-thread"
                               '()
                               'run-and-monitor-progress-background-thread
                               #'update-progress-bar
                               query-text text-pane2 text-pane3
                             ))))

;; This function runs in a separate thread.  The call to sleep
;; simulates a slow operation being done.

(defvar *percent*)

(defun run-and-monitor-progress-background-thread (updater text text-pane2 text-pane3)
  (setf *percent* 0)
  (unwind-protect
      (setf (capi:editor-pane-text text-pane2) "")
    (setf (capi:editor-pane-text text-pane3) "")
    ;;(capi:display-message "done")
    (let ((message-stream (collector-pane-stream text-pane2))
          (results-stream (collector-pane-stream text-pane3)))
      (format message-stream "# Starting to process query....~%")
      (format results-stream *pane3-message*)
      (let ((user-selections (get-entity-data-helper text :updater updater :message-stream message-stream)))
        ;; wrapped-results
        ;; (results
        ;;  (with-output-to-string (results-stream)
        (setf *percent* (+ *percent* 2))
        (funcall updater *percent*)
        (setf (capi:editor-pane-text text-pane3) "")
        (dolist (ev user-selections)
          (if (> (length (cadr ev)) 0)
              (let ()
                (terpri results-stream)
                (capi::write-string-with-properties
                 (format nil "- - - ENTITY TYPE: ~A - - -" (car ev))
                 '(:highlight :compiler-error-highlight) results-stream)
                (terpri results-stream)
                (dolist (uri (cadr ev))
                  (setf uri (car uri))
                  (case (car ev)
                    (:people
                     (pprint-results 
                      (dbpedia-get-person-detail  uri :message-stream message-stream)
                      :stream results-stream))
                    (:companies
                     (pprint-results 
                      (dbpedia-get-company-detail uri :message-stream message-stream)
                      :stream results-stream))
                    (:countries
                     (pprint-results
                      (dbpedia-get-country-detail uri :message-stream message-stream)
                      :stream results-stream))
                    (:cities
                     (pprint-results
                      (dbpedia-get-city-detail    uri :message-stream message-stream)
                      :stream results-stream))
                    (:products
                     (pprint-results
                      (dbpedia-get-product-detail uri :message-stream message-stream)
                      :stream results-stream))))))
          (setf *percent* (+ *percent* 1))
          (funcall updater *percent*))

        (let (links x)
          (dolist (ev user-selections)
            (dolist (uri (second ev))
              (setf uri (car uri))
              (if (> (length ev) 2)
                  (setf x (caddr ev)))
              (setf links (cons (list (symbol-name (first ev)) uri x) links))
              (setf *percent* (+ *percent* 1))
              (funcall updater *percent*)))

          (setf
           links
           (append
            links
            (entity-results->relationship-links
             user-selections
             :message-stream message-stream
             :updater updater)))
          (setf *percent* (+ *percent* 2))
          (funcall updater *percent*)

          (if
              *show-info-pane*
              (lw-grapher:make-info-panel-grapher '("PEOPLE" "COMPANIES" "COUNTRIES" "CITIES" "PRODUCTS" "PLACES")
                                                  links 'test-callback-click 'test-callback-click-shift))))) ;; do  not use #' !!

    (funcall updater 0)))

;; MAIN entry point for application:

(defun kgn ()
  (ignore-errors (create-dbpedia))
  (capi:display (make-instance 'kgn-interface)))

