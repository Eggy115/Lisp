(in-package #:kgn)

(defun pprint-results (results &key (stream t))
  (dolist (result (car results))
    (terpri stream)
    (capi::write-string-with-properties 
     (format nil  "~A:" (first result))
     '(:highlight :compiler-warning-highlight) stream)
    (format stream " ~A~%" (second result))))

(defun clean-comment (comment-string)
  (let ((i1 (search "(" comment-string))
        (i2 (search ")" comment-string)))
    (if (and i1 i2 (> i2 i1) (> i1 0))
        (concatenate 'string (subseq comment-string 0 (- i1 1)) (subseq comment-string (+ i2 1)))
      (let ((j1 (search " / " comment-string)))
        (if j1
            (let ((j2 (search "/" comment-string :start2 (+ j1 2))))
              (if (and j1 j2 (> j2 j1) (< (+ j2 1) (length comment-string)))
                  (concatenate 'string (subseq comment-string 0 j1) (subseq comment-string (+ j2 1)))
                comment-string))
          comment-string)
        comment-string))))

(defun clean-results (results)
  (dolist (result results)
    (setf (second (assoc :comment result))
          (clean-comment (second (assoc :comment result)))))
  results)

(defun get-name-and-description-for-uri (uri)
  (let* ((sparql
          (replace-all
           (format nil "select distinct ?name ?comment { @@ ~
                         values ?nameProperty {<http://www.w3.org/2000/01/rdf-schema#label> <http://xmlns.com/foaf/0.1/name> } . @@ ~
                         ~A ?nameProperty ?name . @@ ~
                         ~A <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment  . FILTER  (lang(?comment) = 'en') . @@ ~
                     } LIMIT 1" uri uri)
           "@@" " "))
         (results (sparql-dbpedia sparql)))
    ;;(print sparql) (print results)
    (list (second (assoc :name (car results))) (second (assoc :comment (car results))))))

;;(print (get-name-and-description-for-uri "<http://dbpedia.org/resource/Apple_Inc.>"))
 
(defun ask-is-type-of (entity-uri type-value-uri) ;; both URIs expected to use surrounding < > brackets for SPARQL
  (let* ((sparql
          (format nil "ASK { ~A <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ~A }"
                  entity-uri type-value-uri))
         (results (sparql-ask-dbpedia sparql)))
    (print sparql)
    results))

;; (print (ask-is-type-of "<http://dbpedia.org/resource/Apple_Inc.>" "<http://dbpedia.org/ontology/Company>"))

(defun dbpedia-get-entities-by-name (name dbpedia-type schema-org-type &key (message-stream t) (updater nil))
  ;; http://www.w3.org/1999/02/22-rdf-syntax-ns#type <http://schema.org/Person>
  (if updater
      (let ()
        (setf *percent* (+ *percent* 2))
        (funcall updater *percent*)))
  (let* ((sparql
         (format nil "select distinct ?s ?comment { ?s ?p \"~A\"@en . @@ ~
                         ?s <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment  . FILTER  (lang(?comment) = 'en') . @@ ~
                         ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ~A . @@ ~
                     } LIMIT 15" name dbpedia-type))
         (results (sparql-dbpedia (replace-all sparql "@@" " "))))
    (terpri message-stream)
    (capi::write-string-with-properties 
     (format nil "Trying to get entity by name = ~A using SPARQL with type:" name dbpedia-type)
     '(:highlight :compiler-warning-highlight) message-stream)
    (terpri message-stream)
    (colorize-sparql sparql :stream message-stream)
    (if (null results)
        (let* ((sparql2
                (format nil "select distinct ?s ?comment { ?s ?p \"~A\"@en . @@ ~
                         ?s <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment  . FILTER  (lang(?comment) = 'en') . @@ ~
                         ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ~A . @@ ~
                     } LIMIT 15" name schema-org-type)))
          (capi::write-string-with-properties 
           (format
            nil
            "No results for ~A for last SPARQL query using type ~A so trying type ~A" name dbpedia-type schema-org-type)
           '(:highlight :compiler-note-highlight) message-stream)
          (terpri message-stream)
          (setf results (sparql-dbpedia (replace-all sparql2 "@@" " ")))
          (if (null results)
              (capi::write-string-with-properties 
               (format
                nil
                "No results for ~A for last SPARQL query using type ~A" name schema-org-type)
               '(:highlight :compiler-note-highlight) message-stream))))
    (let* ((filtered (remove-if
                      #'(lambda (x)
                          (or
                           (search "," (cadar x))
                           (and
                            (not (equal (first x) :comment))
                            (not (search "/resource/" (cadar x))))))
                      results))
           (uris (remove-duplicates
                  (map 'list #'(lambda (x) (list (concatenate 'string "<" (cadar x) ">") (cadadr x)))
                       filtered) :test #'equal)))
      (format t "~%~%********** dbpedia-get-entities-by-name: uris:~%") (pprint uris) (terpri)
      uris)))

(defun dbpedia-get-person-detail (person-uri &key (message-stream t))
  ;; http://dbpedia.org/ontology/birthPlace 
  ;;http://dbpedia.org/ontology/birthDate
  (let* ((query
          (format nil
                 "SELECT DISTINCT ?label ?comment@@ ~
                     (GROUP_CONCAT (DISTINCT ?birthplace; SEPARATOR=' | ') AS ?birthplace) @@ ~
                     (GROUP_CONCAT (DISTINCT ?almamater; SEPARATOR=' | ') AS ?almamater) @@ ~
                     (GROUP_CONCAT (DISTINCT ?spouse; SEPARATOR=' | ') AS ?spouse) { @@ ~
                     ~A <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment .@@
                           FILTER  (lang(?comment) = 'en') . @@ ~
                     OPTIONAL { ~A <http://dbpedia.org/ontology/birthPlace> ?birthplace } . @@ ~
                     OPTIONAL { ~A <http://dbpedia.org/ontology/almaMater> ?almamater } . @@ ~
                     OPTIONAL { ~A <http://dbpedia.org/ontology/spouse> ?spouse } . @@ ~
                     OPTIONAL { ~A  <http://www.w3.org/2000/01/rdf-schema#label> ?label .@@ ~
                             FILTER  (lang(?label) = 'en') } @@ ~
                 } LIMIT 10@@" person-uri person-uri person-uri person-uri person-uri))
         (results (sparql-dbpedia  (replace-all query "@@" " "))))
    (format message-stream "~%SPARQL to get PERSON data for ~A:~%~%" person-uri)
    (colorize-sparql query :stream message-stream)
    (format message-stream "~%")
    (clean-results results)))

(defun dbpedia-get-company-detail (company-uri &key (message-stream t))
  (let* ((query
          (format nil
                  "SELECT DISTINCT ?label ?comment (GROUP_CONCAT (DISTINCT ?industry; SEPARATOR=' | ') AS ?industry)@@ ~
                      (GROUP_CONCAT (DISTINCT ?netIncome; SEPARATOR=' | ') AS ?netIncome)@@ ~
                      (GROUP_CONCAT (DISTINCT ?numberOfEmployees; SEPARATOR=' | ') AS ?numberOfEmployees) {@@ ~
                      ~A <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment .@@
                           FILTER  (lang(?comment) = 'en') .@@ ~
                      OPTIONAL { ~A <http://dbpedia.org/ontology/industry> ?industry } .@@  ~
                      OPTIONAL { ~A <http://dbpedia.org/ontology/netIncome> ?netIncome } .@@  ~
                      OPTIONAL { ~A <http://dbpedia.org/ontology/numberOfEmployees> ?numberOfEmployees } .@@  ~
                      OPTIONAL { ~A <http://www.w3.org/2000/01/rdf-schema#label> ?label . FILTER (lang(?label) = 'en') } @@ ~
                   } LIMIT 30@@"
                  company-uri company-uri company-uri company-uri company-uri))
         (results (sparql-dbpedia  (replace-all query "@@" " "))))
    (format message-stream "~%SPARQL to get COMPANY data for ~A:~%~%" company-uri)
    (colorize-sparql query :stream message-stream)
    (format message-stream "~%")
    (clean-results results)))

(defun dbpedia-get-country-detail (country-uri &key (message-stream t))
  (let* ((query
          (format nil
                  "SELECT DISTINCT ?label ?comment (GROUP_CONCAT (DISTINCT ?areaTotal; SEPARATOR=' | ') AS ?areaTotal)@@ ~
                     (GROUP_CONCAT (DISTINCT ?populationDensity; SEPARATOR=' | ') AS ?populationDensity) {@@ ~
                     ~A <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment .@@
                           FILTER  (lang(?comment) = 'en') .@@ ~
                     OPTIONAL { ~A <http://dbpedia.org/ontology/areaTotal> ?areaTotal } .@@  ~
                     OPTIONAL { ~A <http://dbpedia.org/ontology/populationDensity> ?populationDensity } .@@  ~
                     OPTIONAL { ~A <http://www.w3.org/2000/01/rdf-schema#label> ?label . }@@ ~
                   } LIMIT 30@@"
                  country-uri country-uri country-uri country-uri country-uri))
         (results (sparql-dbpedia  (replace-all query "@@" " "))))
    (format message-stream "~%SPARQL to get COUNTRY data for ~A:~%~%" country-uri)
    (colorize-sparql query :stream message-stream)
    (format message-stream "~%")
    (clean-results results)))


(defun dbpedia-get-city-detail (city-uri &key (message-stream t))
  (let* ((query
          (format
           nil
           "SELECT DISTINCT ?label ?comment @@ ~
                (GROUP_CONCAT (DISTINCT ?latitude_longitude; SEPARATOR=' | ')  AS ?latitude_longitude) @@ ~
                (GROUP_CONCAT (DISTINCT ?populationDensity; SEPARATOR=' | ') AS ?populationDensity) @@ ~
                (GROUP_CONCAT (DISTINCT ?country; SEPARATOR=' | ') AS ?country) { @@ ~
              ~A <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment . FILTER  (lang(?comment) = 'en') . @@ ~
              OPTIONAL { ~A <http://www.w3.org/2003/01/geo/wgs84_pos#geometry> ?latitude_longitude } . @@ ~
              OPTIONAL { ~A <http://dbpedia.org/ontology/PopulatedPlace/populationDensity> ?populationDensity } . @@ ~
              OPTIONAL { ~A <http://dbpedia.org/ontology/country> ?country } .@@ ~
              OPTIONAL { ~A <http://www.w3.org/2000/01/rdf-schema#label> ?label . } @@ ~
            } LIMIT 30@@"
           city-uri city-uri city-uri city-uri city-uri))
         (results (sparql-dbpedia (replace-all query "@@" " "))))
    (format message-stream "~%SPARQL to get CITY data for ~A:~%~%" city-uri)
    (colorize-sparql query :stream message-stream)
    (format message-stream "~%")
    (clean-results results)))

(defun dbpedia-get-product-detail (product-uri &key (message-stream t))
  (let* ((query
          (format
           nil
           "SELECT DISTINCT ?label ?comment {  @@ ~
              ~A <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment . FILTER  (lang(?comment) = 'en') . @@ ~
               OPTIONAL { ~A <http://www.w3.org/2000/01/rdf-schema#label> ?label . } ~
            } LIMIT 30@@"
           product-uri product-uri))
         (results (sparql-dbpedia (replace-all query "@@" " "))))
    (format message-stream "~%SPARQL to get PRODUCT data for ~A:~%~%" product-uri)
    (colorize-sparql query :stream message-stream)
    (format message-stream "~%")
    (clean-results results)))


(defun dbpedia-get-relationships (s-uri o-uri) ;;  &key (message-stream t))
  (let* ((query
          (format
           nil
           "SELECT DISTINCT ?p {  ~A ?p ~A . FILTER (!regex(str(?p), 'wikiPage', 'i')) } LIMIT 5"
           s-uri o-uri))
         (results (sparql-dbpedia query)))
    (alexandria:flatten (map 'list
                             #'(lambda (x)
                                 (format nil "~{<~A>~}" (cdar x)))
                             results))))

(defun entities (text)
  (let ((txt-obj (kbnlp:make-text-object text)))
    (list (kbnlp::text-human-names txt-obj) (kbnlp::text-place-names txt-obj) (kbnlp::text-company-names txt-obj))))

(defun entities-dbpedia (text)
  (let ((e-hash (entity-uris:find-entities-in-text text)))
    (list (gethash "people" e-hash) (gethash "companies" e-hash) (gethash "countries" e-hash) (gethash "cities" e-hash))))


