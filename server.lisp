(defpackage :linkbox
  (:use :cl :hunchentoot :alexandria
   :optima :optima.ppcre)
  (:export #:make-linkbox))
(in-package :linkbox)

;;; The acceptor carries all state ---------------------------------------------

(defclass linkbox-acceptor (acceptor)
  ((id-file      :initarg :id-file      :reader id-file)
   (%id                                 :accessor %id)
   (url          :initarg :url          :reader url)
   (path         :initarg :path         :reader path)
   (auth         :initarg :auth         :reader auth)
   (targets-file :initarg :targets-file :reader targets-file)
   (%targets                            :accessor %targets)))

;;; Maintaining targets --------------------------------------------------------
;;; Targets are saved when a new one is added and only loaded when the server
;;; starts.

(deftype target-type ()
  '(member :file :url))

(defun save-targets (acceptor)
  (with-open-file (stream (targets-file acceptor)
                          :direction :output
                          :if-exists :supersede)
    (print (hash-table-alist (%targets acceptor))
           stream)))

(defun load-targets (acceptor)
  (with-open-file (stream (targets-file acceptor)
                          :direction :input
                          :if-does-not-exist nil)
    (setf (%targets acceptor)
          (alist-hash-table (and stream (read stream nil nil))
                            :test #'equal))))

(defun add-target (from to type &optional (acceptor *acceptor*))
  (declare (type string from)
           (type string to)
           (type target-type type))
  (prog1 from
    (setf (gethash from (%targets acceptor))
          (list type to))
    (save-targets acceptor)))

(defun target (name &optional (acceptor *acceptor*))
  (gethash name (%targets acceptor)))

;;; Maintaining the current ID -------------------------------------------------
;;; The id is saved when a new target is added and only loaded once when the
;;; server starts.

(defun load-id (acceptor)
  (with-open-file (stream (id-file acceptor)
                          :direction :input
                          :if-does-not-exist nil)
    (setf (%id acceptor)
          (if stream
              (or (read stream nil nil) 0)
              0))))

(defun save-id (acceptor)
  (with-open-file (stream (id-file acceptor)
                          :direction :output
                          :if-exists :supersede)
    (print (%id acceptor) stream)))

(defun integer->letters (integer)
  (let* ((characters #.(concatenate 'string
                                    "abcdefghijklmnopqrstuvwxyz"
                                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                    "0123456789"))
         (max (1- (length characters))))
    (coerce (loop while (plusp integer)
                  collect (char characters (mod integer max))
                  do (setf integer (floor integer max)))
            'string)))

(defun next-id (&optional (acceptor *acceptor*))
  (prog1 (integer->letters (incf (%id acceptor)))
    (save-id acceptor)))

;;; Creating targets -----------------------------------------------------------

(defun create-short-url (url)
  (add-target (next-id) url :url))

(defun create-file-url (path file-name content-type
                        &optional (acceptor *acceptor*))
  (declare (ignore content-type))
  (let* ((id (next-id))
         (pathname (parse-namestring file-name))
         (targets-file (format nil "~A~@[.~A~]"
                               id
                               (pathname-type pathname))))
    (rename-file path
                 (merge-pathnames targets-file
                                  (path acceptor)))
    (add-target targets-file targets-file :file)))

;;; Handling requests ----------------------------------------------------------

(defun handle-create (&optional (acceptor *acceptor*))
  (let ((id (cond
              ((stringp (post-parameter "url"))
               (create-short-url (post-parameter "url")))
              ((consp (post-parameter "file"))
               (apply 'create-file-url (post-parameter "file"))))))
    (if id
        (format nil "~A~A"
                (url acceptor)
                id)
        "Failed.")))

(defmethod acceptor-dispatch-request ((acceptor linkbox-acceptor) request)
  (match (script-name*)
    ;; Creation
    ((guard (ppcre "^/$")
            (and (eq (request-method*) :post)
                 (equal (post-parameter "auth")
                        (auth acceptor))))
     (handle-create))
    ;; Redirecting
    ((ppcre "^/(.+)$" name)
     (destructuring-bind (&optional type target)
         (target name)
       (if (and type target)
           (case type
             (:file
              (handle-static-file
               (merge-pathnames target (path acceptor))))
             (:url (redirect target :code +http-see-other+))
             (t (fail)))
           (fail))))
    (_ (setf (return-code*) +http-not-found+)
       nil)))

;;; Convenience ----------------------------------------------------------------

(defun make-linkbox (&key url path auth id-file targets-file
                       (port 5000))
  (assert (stringp url))
  (assert (or (stringp path) (pathnamep path)))
  (assert (stringp auth))
  (assert (or (stringp id-file) (pathnamep id-file)))
  (assert (or (stringp targets-file) (pathnamep targets-file)))
  (ensure-directories-exist path)
  (ensure-directories-exist id-file)
  (ensure-directories-exist targets-file)
  (let ((linkbox (make-instance 'linkbox-acceptor
                                :url url
                                :path path
                                :auth auth
                                :id-file id-file
                                :targets-file targets-file
                                :port port)))
    (prog1 linkbox
      (load-id linkbox)
      (load-targets linkbox))))
