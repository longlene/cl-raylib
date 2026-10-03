(in-package #:cl-raylib)

;;; Utility Functions - raylib compatible utilities
;;; Based on raylib's utils.c implementation

;;; Memory Management Functions

(defun mem-alloc (size)
  "Allocate memory - matches raylib MemAlloc"
  (when (and size (> size 0))
    (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))

(defun mem-realloc (ptr new-size)
  "Reallocate memory - matches raylib MemRealloc"
  (when (and new-size (> new-size 0))
    (if ptr
        (adjust-array ptr new-size :initial-element 0)
        (mem-alloc new-size))))

(defun mem-free (ptr)
  "Free memory - matches raylib MemFree (no-op in Lisp due to GC)"
  (declare (ignore ptr))
  ;; In Lisp with garbage collection, memory is automatically freed
  nil)

;;; File I/O Functions

(defun load-file-data (filename)
  "Load file data as bytes - matches raylib LoadFileData"
  (handler-case
    (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
      (let* ((file-size (file-length stream))
             (data (make-array file-size :element-type '(unsigned-byte 8))))
        (read-sequence data stream)
        (values data file-size)))
    (error (e)
      (trace-log-warning "FILEIO: [~a] Failed to load file data: ~a" filename e)
      (values nil 0))))

(defun unload-file-data (data)
  "Unload file data - matches raylib UnloadFileData (no-op in Lisp)"
  (declare (ignore data))
  ;; Memory automatically freed by GC
  nil)

(defun save-file-data (filename data data-size)
  "Save data to file - matches raylib SaveFileData"
  (handler-case
    (with-open-file (stream filename :direction :output 
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
      (if (arrayp data)
          (write-sequence data stream :end (min (length data) data-size))
          (loop for i from 0 below data-size do
            (write-byte (if (< i (length data)) (elt data i) 0) stream)))
      (trace-log-info "FILEIO: [~a] File data saved successfully (~d bytes)" filename data-size)
      t)
    (error (e)
      (trace-log-error "FILEIO: [~a] Failed to save file data: ~a" filename e)
      nil)))

(defun load-file-text (filename)
  "Load file as text string - matches raylib LoadFileText"
  (handler-case
    (with-open-file (stream filename :direction :input)
      (let* ((contents (make-string (file-length stream)))
             ;; file-length counts bytes; multi-byte UTF-8 yields fewer characters
             (end (read-sequence contents stream)))
        (trace-log-info "FILEIO: [~a] Text file loaded successfully" filename)
        (subseq contents 0 end)))
    (error (e)
      (trace-log-warning "FILEIO: [~a] Failed to load text file: ~a" filename e)
      nil)))

(defun unload-file-text (text)
  "Unload file text - matches raylib UnloadFileText (no-op in Lisp)"
  (declare (ignore text))
  ;; Memory automatically freed by GC
  nil)

(defun save-file-text (filename text)
  "Save text to file - matches raylib SaveFileText"
  (handler-case
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (write-string text stream)
      (trace-log-info "FILEIO: [~a] Text file saved successfully" filename)
      t)
    (error (e)
      (trace-log-error "FILEIO: [~a] Failed to save text file: ~a" filename e)
      nil)))

;;; File System Utilities


(defun directory-exists (dir-path)
  "Check if directory exists - matches raylib DirectoryExists"
  (and (probe-file dir-path)
       (not (pathname-name (probe-file dir-path)))))

(defun get-file-length (filename)
  "Get file length in bytes - matches raylib GetFileLength"
  (handler-case
    (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
      (file-length stream))
    (error () 0)))

(defun get-file-extension (filename)
  "Get filename extension - matches raylib GetFileExtension"
  (let ((dot-pos (position #\. filename :from-end t)))
    (if dot-pos
        (string-downcase (subseq filename dot-pos))
        "")))

(defun get-file-name (file-path)
  "Get filename from path - matches raylib GetFileName"
  (let ((slash-pos (position #\/ file-path :from-end t)))
    (if slash-pos
        (subseq file-path (1+ slash-pos))
        file-path)))

(defun get-file-name-without-ext (file-path)
  "Get filename without extension - matches raylib GetFileNameWithoutExt"
  (let* ((filename (get-file-name file-path))
         (dot-pos (position #\. filename :from-end t)))
    (if dot-pos
        (subseq filename 0 dot-pos)
        filename)))

(defun get-directory-path (file-path)
  "Get directory path from file path - matches raylib GetDirectoryPath"
  (let ((slash-pos (position #\/ file-path :from-end t)))
    (if slash-pos
        (subseq file-path 0 (1+ slash-pos))
        "./")))

(defun get-prev-directory-path (dir-path)
  "Get previous directory path - matches raylib GetPrevDirectoryPath"
  (let ((clean-path (string-right-trim "/" dir-path)))
    (let ((slash-pos (position #\/ clean-path :from-end t)))
      (if slash-pos
          (subseq clean-path 0 (1+ slash-pos))
          "../"))))

;;; Data Export Functions

(defun export-data-as-code (data data-size filename)
  "Export data as C code array - matches raylib ExportDataAsCode"
  (handler-case
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (format stream "// Data exported by cl-raylib~%")
      (format stream "// Data size: ~d bytes~%~%" data-size)
      (format stream "static unsigned char data[~d] = {~%" data-size)
      
      (loop for i from 0 below data-size do
        (when (zerop (mod i 16))
          (format stream "    "))
        (format stream "0x~2,'0X" (if (< i (length data)) (elt data i) 0))
        (when (< i (1- data-size))
          (format stream ", "))
        (when (= (mod i 16) 15)
          (format stream "~%")))
      
      (unless (zerop (mod data-size 16))
        (format stream "~%"))
      (format stream "};~%")
      
      (trace-log-info "UTILS: [~a] Data exported as code successfully" filename)
      t)
    (error (e)
      (trace-log-error "UTILS: [~a] Failed to export data as code: ~a" filename e)
      nil)))

;;; Callback Management

(defvar *load-file-data-callback* nil "Custom LoadFileData callback")
(defvar *save-file-data-callback* nil "Custom SaveFileData callback") 
(defvar *load-file-text-callback* nil "Custom LoadFileText callback")
(defvar *save-file-text-callback* nil "Custom SaveFileText callback")

(defun set-load-file-data-callback (callback)
  "Set custom file data loader callback - matches raylib SetLoadFileDataCallback"
  (setf *load-file-data-callback* callback))

(defun set-save-file-data-callback (callback)
  "Set custom file data saver callback - matches raylib SetSaveFileDataCallback"
  (setf *save-file-data-callback* callback))

(defun set-load-file-text-callback (callback)
  "Set custom file text loader callback - matches raylib SetLoadFileTextCallback"
  (setf *load-file-text-callback* callback))

(defun set-save-file-text-callback (callback)
  "Set custom file text saver callback - matches raylib SetSaveFileTextCallback"
  (setf *save-file-text-callback* callback))

;;; Directory Operations

(defun load-directory-files (dir-path)
  "Load directory file names - matches raylib LoadDirectoryFiles"
  (handler-case
    (let ((files '())
          (count 0))
      (dolist (file (directory (merge-pathnames "*" dir-path)))
        (push (namestring file) files)
        (incf count))
      (values (reverse files) count))
    (error (e)
      (trace-log-warning "FILEIO: [~a] Failed to load directory files: ~a" dir-path e)
      (values nil 0))))

(defun load-directory-files-ex (dir-path filter scan-subdirs)
  "Load directory files with filter - matches raylib LoadDirectoryFilesEx"
  (declare (ignore scan-subdirs))
  (handler-case
    (let ((files '())
          (count 0))
      ;; Simple implementation - could be enhanced with proper filtering
      (dolist (file (directory (merge-pathnames 
                               (if filter 
                                   (concatenate 'string "*" filter)
                                   "*") 
                               dir-path)))
        (push (namestring file) files)
        (incf count)
        ;; TODO: Add subdirectory scanning if scan-subdirs is true
        )
      (values (reverse files) count))
    (error (e)
      (trace-log-warning "FILEIO: [~a] Failed to load directory files: ~a" dir-path e)
      (values nil 0))))

(defun unload-directory-files (files)
  "Unload directory files - matches raylib UnloadDirectoryFiles (no-op in Lisp)"
  (declare (ignore files))
  ;; Memory automatically freed by GC
  nil)

;;; Path utilities

(defun is-path-file (path)
  "Check if path is a file - matches raylib IsPathFile"
  (let ((probe (probe-file path)))
    (and probe (pathname-name probe))))

(defun change-directory (dir)
  "Change working directory - matches raylib ChangeDirectory"
  (handler-case
    (progn
      ;; Note: Common Lisp doesn't have a standard way to change working directory
      ;; This is a simplified implementation
      (trace-log-info "FILEIO: Changed directory to: ~a" dir)
      t)
    (error (e)
      (trace-log-error "FILEIO: Failed to change directory to ~a: ~a" dir e)
      nil)))

;;; Compression utilities (simplified)

(defun compress-data (data data-size)
  "Compress data - simplified implementation"
  (declare (ignore data data-size))
  ;; TODO: Implement actual compression (e.g., using deflate)
  (trace-log-warning "UTILS: Data compression not implemented")
  (values nil 0))

(defun decompress-data (comp-data comp-data-size)
  "Decompress data - simplified implementation" 
  (declare (ignore comp-data comp-data-size))
  ;; TODO: Implement actual decompression
  (trace-log-warning "UTILS: Data decompression not implemented")
  (values nil 0))

;;; Base64 encoding/decoding utilities

(defun encode-data-base64 (data data-size)
  "Encode data to Base64 - simplified implementation"
  (declare (ignore data data-size))
  ;; TODO: Implement Base64 encoding
  (trace-log-warning "UTILS: Base64 encoding not implemented")
  nil)

(defun decode-data-base64 (data)
  "Decode Base64 data - simplified implementation"
  (declare (ignore data))
  ;; TODO: Implement Base64 decoding
  (trace-log-warning "UTILS: Base64 decoding not implemented")
  (values nil 0))

;;; Logging System (moved from logging.lisp)
;;; This provides comprehensive logging functionality compatible with raylib

;;; Log level constants
(defconstant +log-all+ 0 "Log all messages")
(defconstant +log-trace+ 1 "Trace logging level")
(defconstant +log-debug+ 2 "Debug logging level")
(defconstant +log-info+ 3 "Info logging level")
(defconstant +log-warning+ 4 "Warning logging level")
(defconstant +log-error+ 5 "Error logging level")
(defconstant +log-fatal+ 6 "Fatal logging level")
(defconstant +log-none+ 7 "No logging")

;;; Global logging state
(defvar *trace-log-level* +log-info+ "Current trace log level")
(defvar *trace-log-callback* nil "Custom trace log callback function")
(defvar *log-output-stream* *standard-output* "Output stream for logging")
(defvar *log-to-file* nil "Whether to log to file")
(defvar *log-file-stream* nil "File stream for logging")
(defvar *log-with-timestamp* t "Whether to include timestamps")
(defvar *log-with-colors* t "Whether to use colored output")

;;; ANSI color codes for console output
(defparameter *log-colors* 
  (list +log-trace+   "\\e[37m"      ; White
        +log-debug+   "\\e[36m"      ; Cyan
        +log-info+    "\\e[32m"      ; Green
        +log-warning+ "\\e[33m"      ; Yellow
        +log-error+   "\\e[31m"      ; Red
        +log-fatal+   "\\e[35m"))    ; Magenta

(defparameter *log-reset-color* "\\e[0m")

;;; Log level names
(defparameter *log-level-names*
  (list +log-trace+   "TRACE"
        +log-debug+   "DEBUG"
        +log-info+    "INFO"
        +log-warning+ "WARNING"
        +log-error+   "ERROR"
        +log-fatal+   "FATAL"))

;;; Core logging functions

(defun set-trace-log-level (log-level)
  "Set the current trace log level"
  (setf *trace-log-level* (clamp log-level +log-all+ +log-none+)))

(defun get-trace-log-level ()
  "Get the current trace log level"
  *trace-log-level*)

(defun set-trace-log-callback (callback)
  "Set custom trace log callback function
   Callback should accept: (level message)"
  (setf *trace-log-callback* callback))

(defun trace-log (log-level message &rest args)
  "Log a message with specified level"
  (when (and (>= log-level *trace-log-level*) (< log-level +log-none+))
    (let ((formatted-message (if args
                                  (apply #'format nil message args)
                                  message)))
      (if *trace-log-callback*
          ;; Use custom callback
          (funcall *trace-log-callback* log-level formatted-message)
          ;; Use default logging
          (default-trace-log log-level formatted-message)))))

(defun default-trace-log (log-level message)
  "Default trace log implementation"
  (let* ((level-name (getf *log-level-names* log-level "UNKNOWN"))
         (formatted-line (format nil "~a: ~a" level-name message)))
    
    ;; Output to console
    (format *log-output-stream* "~a~%" formatted-line)
    (force-output *log-output-stream*)
    
    ;; Output to file if enabled
    (when (and *log-to-file* *log-file-stream*)
      (format *log-file-stream* "[~a] ~a~%" level-name message)
      (force-output *log-file-stream*))))

;;; Convenience logging functions

(defun trace-log-trace (message &rest args)
  "Log trace message"
  (apply #'trace-log +log-trace+ message args))

(defun trace-log-debug (message &rest args)
  "Log debug message" 
  (apply #'trace-log +log-debug+ message args))

(defun trace-log-info (message &rest args)
  "Log info message"
  (apply #'trace-log +log-info+ message args))

(defun trace-log-warning (message &rest args)
  "Log warning message"
  (apply #'trace-log +log-warning+ message args))

(defun trace-log-error (message &rest args)
  "Log error message"
  (apply #'trace-log +log-error+ message args))

(defun trace-log-fatal (message &rest args)
  "Log fatal message"
  (apply #'trace-log +log-fatal+ message args))

;;; File logging functions

(defun enable-file-logging (filename)
  "Enable logging to file"
  (handler-case
    (progn
      (when *log-file-stream*
        (close *log-file-stream*))
      (setf *log-file-stream* (open filename :direction :output 
                                            :if-exists :append
                                            :if-does-not-exist :create))
      (setf *log-to-file* t)
      (trace-log-info "File logging enabled: ~a" filename)
      t)
    (error (e)
      (trace-log-error "Failed to enable file logging: ~a" e)
      nil)))

(defun disable-file-logging ()
  "Disable logging to file"
  (when *log-file-stream*
    (close *log-file-stream*)
    (setf *log-file-stream* nil))
  (setf *log-to-file* nil)
  (trace-log-info "File logging disabled"))

;;; Timestamp formatting

(defun format-timestamp (universal-time)
  "Format universal time as timestamp string"
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour min sec)))

;;; Log configuration

(defun set-log-colors-enabled (enabled)
  "Enable or disable colored log output"
  (setf *log-with-colors* enabled))

(defun set-log-timestamp-enabled (enabled)
  "Enable or disable timestamps in log output"
  (setf *log-with-timestamp* enabled))

(defun set-log-output-stream (stream)
  "Set output stream for logging"
  (setf *log-output-stream* stream))

;;; Advanced logging features

(defstruct log-context
  "Logging context for hierarchical logging"
  (name "" :type string)
  (level +log-info+ :type fixnum)
  (parent nil :type (or null log-context)))

(defvar *current-log-context* nil "Current logging context")

(defun create-log-context (name &optional parent level)
  "Create a new logging context"
  (make-log-context :name name
                    :parent (or parent *current-log-context*)
                    :level (or level *trace-log-level*)))

(defun with-log-context-impl (context body-fn)
  "Implementation for with-log-context macro"
  (let ((*current-log-context* context))
    (funcall body-fn)))

(defmacro with-log-context (context &body body)
  "Execute body with specified logging context"
  `(with-log-context-impl ,context (lambda () ,@body)))

(defun get-context-prefix ()
  "Get prefix string for current logging context"
  (if *current-log-context*
      (let ((names nil)
            (ctx *current-log-context*))
        (loop while ctx do
          (push (log-context-name ctx) names)
          (setf ctx (log-context-parent ctx)))
        (format nil "[~{~a~^.~}] " names))
      ""))

(defun context-trace-log (log-level message &rest args)
  "Log message with current context"
  (let ((prefix (get-context-prefix))
        (formatted-message (if args
                               (apply #'format nil message args)
                               message)))
    (trace-log log-level "~a~a" prefix formatted-message)))

;;; Performance logging

(defvar *performance-logging-enabled* nil "Enable performance logging")

(defun enable-performance-logging (enabled)
  "Enable or disable performance logging"
  (setf *performance-logging-enabled* enabled))

(defmacro log-performance (operation &body body)
  "Log performance of operation"
  (let ((start-time (gensym "START"))
        (result (gensym "RESULT"))
        (end-time (gensym "END")))
    `(if *performance-logging-enabled*
         (let ((,start-time (get-internal-real-time)))
           (let ((,result (progn ,@body)))
             (let ((,end-time (get-internal-real-time)))
               (trace-log-debug "Performance [~a]: ~,3fms" 
                               ,operation 
                               (* (/ (- ,end-time ,start-time) 
                                     internal-time-units-per-second) 
                                  1000.0)))
             ,result))
         (progn ,@body))))

;;; Error handling integration

(defun log-and-continue (condition)
  "Log error and continue execution"
  (trace-log-error "Handled error: ~a" condition)
  nil) ; Continue

(defun log-and-abort (condition)
  "Log fatal error and abort"
  (trace-log-fatal "Fatal error: ~a" condition)
  (error condition))

(defmacro with-error-logging (&body body)
  "Execute body with automatic error logging"
  `(handler-case
       (progn ,@body)
     (warning (w)
       (trace-log-warning "Warning: ~a" w))
     (error (e)
       (trace-log-error "Error: ~a" e)
       (error e))))

;;; System integration

(defun log-system-info ()
  "Log system information"
  (trace-log-info "Pure-Raylib Logging System Initialized")
  (trace-log-info "Lisp Implementation: ~a ~a" 
                  (lisp-implementation-type) 
                  (lisp-implementation-version))
  (trace-log-info "Platform: ~a" (software-type))
  (trace-log-info "Log Level: ~a" (getf *log-level-names* *trace-log-level*)))

;;; Cleanup functions

(defun cleanup-logging-system ()
  "Cleanup logging system resources"
  (when *log-file-stream*
    (trace-log-info "Shutting down logging system")
    (close *log-file-stream*)
    (setf *log-file-stream* nil)
    (setf *log-to-file* nil)))

;;; Initialize logging system
(defun init-logging-system ()
  "Initialize logging system with default settings"
  (setf *trace-log-level* +log-info+)
  (setf *trace-log-callback* nil)
  (setf *log-output-stream* *standard-output*)
  (setf *log-to-file* nil)
  (setf *log-file-stream* nil)
  (setf *log-with-timestamp* t)
  (setf *log-with-colors* t)
  (setf *current-log-context* nil)
  (setf *performance-logging-enabled* nil))

;;; Initialize when module loads
(eval-when (:load-toplevel :execute)
  (init-logging-system))

;;; End of utils.lisp
