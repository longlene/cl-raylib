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

;; File access custom callbacks
(defvar *load-file-data-callback* nil "Custom LoadFileData callback")
(defvar *save-file-data-callback* nil "Custom SaveFileData callback")
(defvar *load-file-text-callback* nil "Custom LoadFileText callback")
(defvar *save-file-text-callback* nil "Custom SaveFileText callback")


(defun load-file-data (filename)
  "Load file data as byte array (read), returns (values data data-size), NIL on failure"
  (unless filename
    (trace-log +log-warning+ "FILEIO: File name provided is not valid")
    (return-from load-file-data (values nil 0)))
  (when *load-file-data-callback* (return-from load-file-data (funcall *load-file-data-callback* filename)))
  (let ((stream (ignore-errors (open filename :direction :input :element-type '(unsigned-byte 8)))))
    (unless stream
      (trace-log +log-warning+ "FILEIO: [~a] Failed to open file" filename)
      (return-from load-file-data (values nil 0)))
    (with-open-stream (stream stream)
      (let ((size (file-length stream)))
        (if (> size 0)
            (let* ((data (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
                   (count (read-sequence data stream)))
              (if (/= count size)
                  (trace-log +log-warning+ "FILEIO: [~a] File partially loaded (~d bytes out of ~d)" filename count size)
                  (trace-log +log-info+ "FILEIO: [~a] File loaded successfully" filename))
              (values (if (/= count size) (subseq data 0 count) data) count))
            (progn
              (trace-log +log-warning+ "FILEIO: [~a] Failed to read file" filename)
              (values nil 0)))))))

(defun unload-file-data (data)
  "Unload file data - matches raylib UnloadFileData (no-op in Lisp)"
  (declare (ignore data))
  ;; Memory automatically freed by GC
  nil)

(defun save-file-data (filename data data-size)
  "Save data to file from byte array (write), returns true on success"
  (unless filename
    (trace-log +log-warning+ "FILEIO: File name provided is not valid")
    (return-from save-file-data nil))
  (when *save-file-data-callback* (return-from save-file-data (funcall *save-file-data-callback* filename data data-size)))
  (let ((stream (ignore-errors (open filename :direction :output :if-exists :supersede :if-does-not-exist :create
                                              :element-type '(unsigned-byte 8)))))
    (unless stream
      (trace-log +log-warning+ "FILEIO: [~a] Failed to open file" filename)
      (return-from save-file-data nil))
    (let ((count (handler-case (progn (write-sequence data stream :end (min (length data) data-size))
                                      (min (length data) data-size))
                   (error () 0))))
      (cond ((zerop count) (trace-log +log-warning+ "FILEIO: [~a] Failed to write file" filename))
            ((/= count data-size) (trace-log +log-warning+ "FILEIO: [~a] File partially written" filename))
            (t (trace-log +log-info+ "FILEIO: [~a] File saved successfully" filename)))
      (handler-case (progn (close stream) t)
        (error () nil)))))

(defun load-file-text (filename)
  "Load text data from file (read), returns the text as a string (bytes decoded as UTF-8), NIL on failure"
  (unless filename
    (trace-log +log-warning+ "FILEIO: File name provided is not valid")
    (return-from load-file-text nil))
  (when *load-file-text-callback* (return-from load-file-text (funcall *load-file-text-callback* filename)))
  (let ((stream (ignore-errors (open filename :direction :input :element-type '(unsigned-byte 8)))))
    (unless stream
      (trace-log +log-warning+ "FILEIO: [~a] Failed to open text file" filename)
      (return-from load-file-text nil))
    (with-open-stream (stream stream)
      (let ((size (file-length stream)))
        (if (> size 0)
            (let* ((bytes (make-array size :element-type '(unsigned-byte 8)))
                   (count (read-sequence bytes stream))
                   ;; The text ends at the first NUL character, like the C string
                   (end (or (position 0 bytes :end count) count)))
              (trace-log +log-info+ "FILEIO: [~a] Text file loaded successfully" filename)
              (babel:octets-to-string bytes :end end :encoding :utf-8 :errorp nil))
            (progn
              (trace-log +log-warning+ "FILEIO: [~a] Failed to read text file" filename)
              nil))))))

(defun unload-file-text (text)
  "Unload file text - matches raylib UnloadFileText (no-op in Lisp)"
  (declare (ignore text))
  ;; Memory automatically freed by GC
  nil)

(defun save-file-text (filename text)
  "Save text data to file (write), string encoded as UTF-8, returns true on success"
  (unless filename
    (trace-log +log-warning+ "FILEIO: File name provided is not valid")
    (return-from save-file-text nil))
  (when *save-file-text-callback* (return-from save-file-text (funcall *save-file-text-callback* filename text)))
  (let ((stream (ignore-errors (open filename :direction :output :if-exists :supersede :if-does-not-exist :create
                                              :element-type '(unsigned-byte 8)))))
    (unless stream
      (trace-log +log-warning+ "FILEIO: [~a] Failed to open text file" filename)
      (return-from save-file-text nil))
    (if (handler-case (progn (write-sequence (babel:string-to-octets text :encoding :utf-8) stream) t)
          (error () nil))
        (trace-log +log-info+ "FILEIO: [~a] Text file saved successfully" filename)
        (trace-log +log-warning+ "FILEIO: [~a] Failed to write text file" filename))
    (handler-case (progn (close stream) t)
      (error () nil))))

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

(defun get-file-extension (file-name)
  "Get pointer to extension for a filename string (includes dot: '.png'), NIL if there is none"
  (let ((dot (position #\. file-name :from-end t)))
    (if (or (null dot) (= dot 0))
        nil
        (subseq file-name dot))))

(defun %strprbrk (text charset)
  "String pointer reverse break: position of the right-most occurrence of CHARSET in TEXT"
  (position-if (lambda (c) (find c charset)) text :from-end t))

(defun get-file-name (file-path)
  "Get pointer to filename for a path string"
  (let ((slash (and file-path (%strprbrk file-path "\\/"))))
    (if slash (subseq file-path (1+ slash)) file-path)))

(defun is-file-extension (file-name ext)
  "Check file extension (recommended include point: .png, .wav), EXT can be a list separated by ';'"
  (let ((file-ext (get-file-extension file-name)))
    (when file-ext
      (flet ((lower (string) (map 'string (lambda (c) (if (char<= #\A c #\Z) (char-downcase c) c)) string)))
        ;; NOTE: char fileExtLower[16]: up to 15 characters are compared
        (let ((file-ext-lower (lower (subseq file-ext 0 (min 15 (length file-ext)))))
              ;; MAX_FILE_EXTENSIONS 32: the last one keeps the remaining text
              (ext-list (let ((parts (or (uiop:split-string (lower ext) :separator ";") (list ""))))
                          (if (> (length parts) 32)
                              (append (subseq parts 0 31) (list (format nil "~{~a~^;~}" (nthcdr 31 parts))))
                              parts))))
          (loop for e in ext-list
                ;; Consider the case where extension provided does not start with the '.'
                thereis (string= (if (and (plusp (length e)) (char= (char e 0) #\.))
                                     file-ext-lower
                                     (subseq file-ext-lower 1))
                                 e)))))))

(defun get-file-name-without-ext (file-path)
  "Get filename string without extension"
  (if file-path
      (let* ((file-name (get-file-name file-path))
             ;; Reverse search '.', a leading '.' is kept
             (dot (position #\. file-name :from-end t :start (min 1 (length file-name)))))
        (if dot (subseq file-name 0 dot) file-name))
      ""))

(defun get-directory-path (file-path)
  "Get full path for a given fileName with path (uses static string)"
  ;; In case provided path does not contain a root drive letter (C:\, D:\)
  ;; nor leading path separator (\, /), add the current directory path to dirPath
  (let* ((relative (and (not (and (> (length file-path) 1) (char= (char file-path 1) #\:)))
                        (not (and (> (length file-path) 0) (member (char file-path 0) '(#\\ #\/))))))
         (last-slash (position-if (lambda (c) (member c '(#\\ #\/))) file-path :from-end t)))
    (cond ((null last-slash) (if relative "./" ""))
          ;; The last and only slash is the leading one: path is in a root directory
          ((= last-slash 0) (subseq file-path 0 1))
          (t (concatenate 'string (if relative "./" "") (subseq file-path 0 last-slash))))))

(defun get-prev-directory-path (dir-path)
  "Get previous directory path for a given path"
  (let ((last-index (1- (length dir-path)))
        (is-file (is-path-file dir-path)))
    (loop with i = last-index
          while (>= i 0)
          do (when (find (char dir-path i) "\\/")
               (block separator
                 ;; If this character is a leading '/' (e.g. the '/' in "/usr") or
                 ;; part of a drive root (e.g. "C:\"), include it with the result
                 (cond ((or (= i 0) (and (= i 2) (char= (char dir-path 1) #\:))) (incf i))
                       ;; If this character is a trailing path separator, continue
                       ((= i last-index) (return-from separator)))
                 (if (not is-file)
                     (return-from get-prev-directory-path (subseq dir-path 0 i))
                     (setf is-file nil))))
             (decf i))
    ""))

;;; Data Export Functions

(defun export-data-as-code (data data-size file-name)
  "Export data to code (.h), returns true on success"
  (let* ((var-file-name (map 'string (lambda (c)
                                       (cond ((char<= #\a c #\z) (char-upcase c)) ; Convert variable name to uppercase
                                             ;; Replace non valid character for C identifier with '_'
                                             ((find c ".-?!+") #\_)
                                             (t c)))
                             (get-file-name-without-ext file-name)))
         (txt-data
           (with-output-to-string (s)
             (format s "////////////////////////////////////////////////////////////////////////////////////////~%")
             (format s "//                                                                                    //~%")
             (format s "// DataAsCode exporter v1.0 - Raw data exported as an array of bytes                  //~%")
             (format s "//                                                                                    //~%")
             (format s "// more info and bugs-report:  github.com/raysan5/raylib                              //~%")
             (format s "// feedback and support:       ray[at]raylib.com                                      //~%")
             (format s "//                                                                                    //~%")
             (format s "// Copyright (c) 2022-2026 Ramon Santamaria (@raysan5)                                //~%")
             (format s "//                                                                                    //~%")
             (format s "////////////////////////////////////////////////////////////////////////////////////////~%~%")
             (format s "#define ~a_DATA_SIZE     ~d~%~%" var-file-name data-size)
             (format s "static unsigned char ~a_DATA[~a_DATA_SIZE] = { " var-file-name var-file-name)
             (dotimes (i (1- data-size))
               (format s (if (zerop (mod i 20)) "0x~(~x~),~%" "0x~(~x~), ") (aref data i)))
             (format s "0x~(~x~) };~%" (aref data (1- data-size)))))
         (result (save-file-text file-name txt-data)))
    (if result
        (trace-log +log-info+ "FILEIO: [~a] Data as code exported successfully" file-name)
        (trace-log +log-warning+ "FILEIO: [~a] Failed to export data as code" file-name))
    result))

;;; Callback Management


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

;;; Directory Operations (raylib rcore.c FileSystem functions)

;; File and directory scan filters
;; WARNING: Custom file filters can be specified but following raylib IsFileExtension() convention: ".png;.wav;.glb"
(defparameter +file-filter-tag-all+ "*.*" "Filter to include all file types and directories on scan")
(defparameter +file-filter-tag-file-only+ "FILES*" "Filter to include all file types on scan (no directories)")
(defparameter +file-filter-tag-dir-only+ "DIRS*" "Filter to include only directories on scan")

;; NOTE: struct dirent d_name offset on Linux glibc 64-bit (d_ino, d_off, d_reclen, d_type, d_name)
(defconstant +dirent-name-offset+ 19)

(defun %directory-entries (base-path)
  "Entries of BASE-PATH (excluding . and ..) in readdir() order as (path-string . directory-p),
path is base-path/name and directory-p is (not IsPathFile(path)) like ScanDirectoryFiles()"
  (let ((dir (cffi:foreign-funcall "opendir" :string base-path :pointer))
        (entries '()))
    (unless (cffi:null-pointer-p dir)
      (unwind-protect
           (loop for dp = (cffi:foreign-funcall "readdir" :pointer dir :pointer)
                 until (cffi:null-pointer-p dp)
                 do (let ((name (cffi:foreign-string-to-lisp (cffi:inc-pointer dp +dirent-name-offset+) :encoding :utf-8)))
                      (unless (or (string= name ".") (string= name ".."))
                        (let ((path (format nil "~a/~a" base-path name)))
                          (push (cons path (not (is-path-file path))) entries)))))
        (cffi:foreign-funcall "closedir" :pointer dir :int)))
    (nreverse entries)))

(defun %scan-directory-files (base-path filter scan-subdirs)
  "Paths scanned like raylib ScanDirectoryFiles()/GetDirectoryFileCountEx()"
  (if (directory-exists base-path)
      (loop for (path . directory-p) in (%directory-entries base-path)
            if (not directory-p)
              when (or (null filter) (search +file-filter-tag-all+ filter)
                       (search +file-filter-tag-file-only+ filter) (is-file-extension path filter))
                collect path
              end
            else
              when (and filter (or (search +file-filter-tag-all+ filter) (search +file-filter-tag-dir-only+ filter)))
                collect path
              end
              and when scan-subdirs
                    append (%scan-directory-files path filter scan-subdirs))
      (progn (trace-log-warning "FILEIO: Directory cannot be opened (~a)" base-path)
             nil)))

(defun load-directory-files (dir-path)
  "Load directory filepaths, files and directories, no subdirs scan"
  (load-directory-files-ex dir-path +file-filter-tag-all+ nil))

;; Use "*.*" to include all files and directories on scan
;; Use "FILES*" to include only files on scan
;; Use "DIRS*" to include only directories on scan
(defun load-directory-files-ex (base-path filter scan-subdirs)
  "Load directory filepaths with extension filtering and recursive directory scan"
  (let ((files (make-file-path-list)))
    (if (directory-exists base-path)
        (let* ((filter (if (and filter (string= filter "")) nil filter))
               (paths (%scan-directory-files base-path filter scan-subdirs)))
          (setf (file-path-list-paths files) paths
                (file-path-list-count files) (length paths)
                (file-path-list-capacity files) (length paths)))
        (trace-log-warning "FILEIO: Directory cannot be opened (~a)" base-path))
    files))

(defun unload-directory-files (files)
  "Unload filepaths"
  (declare (ignore files))
  ;; NOTE: Memory is managed by the GC
  nil)

(defun get-directory-file-count (dir-path)
  "Get the file count in a directory"
  (get-directory-file-count-ex dir-path +file-filter-tag-all+ nil))

;; Use 'DIRS*' in the filter string to include directories in the result
(defun get-directory-file-count-ex (base-path filter scan-subdirs)
  "Get the file count in a directory with extension filtering and recursive directory scan"
  (length (%scan-directory-files base-path filter scan-subdirs)))

;;; Path utilities

(defun file-exists (file-name)
  "Check if file exists"
  (and file-name (probe-file file-name) t))

(defun is-file-hidden (file-path)
  "Check if file path (file or directory) is hidden by OS"
  (let* ((slash (position #\/ file-path :from-end t))
         (base-path (if slash (subseq file-path (1+ slash)) file-path)))
    (and (plusp (length base-path))
         (char= (char base-path 0) #\.)
         (string/= base-path ".")
         (string/= base-path ".."))))

(defun get-file-mod-time (file-name)
  "Get file modification time (last write time), as Unix time"
  (let ((write-date (and (probe-file file-name) (file-write-date file-name))))
    (if write-date
        (- write-date 2208988800)       ; Universal time to Unix time
        0)))

(defun is-path-file (path)
  "Check if a given path is a file or a directory"
  (and (uiop:file-exists-p path)
       (not (uiop:directory-exists-p path))
       t))

(defun is-path-directory (path)
  "Check if a given path point to a directory (raylib: any path that is not a regular file)"
  (not (is-path-file path)))

(defun is-path-absolute (path)
  "Check if provided path is an absolute path"
  (and path (plusp (length path)) (char= (char path 0) #\/)))

(defun is-file-name-valid (file-name)
  "Check if fileName is valid for the platform/OS"
  (let ((valid t))
    (when (and file-name (plusp (length file-name)))
      (let ((all-periods t))
        (loop for ch across file-name
              ;; Check invalid characters and non-glyph characters
              do (when (or (find ch "<>:\"/\\|?*") (< (char-code ch) 32))
                   (setf valid nil)
                   (loop-finish))
                 ;; Check if filename is not all periods
                 (unless (char= ch #\.) (setf all-periods nil)))
        (when all-periods (setf valid nil))))
    valid))

;; Create directories (including full path requested), returns 0 on success
(defun make-directory (dir-path)
  "Create directories (including full path requested), returns 0 on success"
  (cond ((or (null dir-path) (string= dir-path "")) -1)        ; Path is not valid
        ((directory-exists dir-path) 0)                       ; Path already exists (is valid)
        (t (handler-case (progn (ensure-directories-exist (uiop:ensure-directory-pathname dir-path))
                                (if (directory-exists dir-path) 0 -1))
             (error () -1)))))

(defun change-directory (dir-path)
  "Change working directory, return 0 on success"
  (if (directory-exists dir-path)
      (let ((dir (uiop:ensure-directory-pathname (truename dir-path))))
        (uiop:chdir dir)
        (setf *default-pathname-defaults* dir)
        (trace-log-info "SYSTEM: Working Directory: ~a" dir-path)
        0)
      (progn (trace-log-warning "SYSTEM: Failed to change to directory: ~a" dir-path)
             -1)))

;; NOTE: Only rename file name required, not full path
(defun file-rename (file-name file-rename)
  "Rename file (if exists), returns 0 on success"
  (if (file-exists file-name)
      (cffi:foreign-funcall "rename" :string file-name :string file-rename :int)
      -1))

(defun file-remove (file-name)
  "Remove file (if exists), returns 0 on success"
  (if (file-exists file-name)
      (cffi:foreign-funcall "remove" :string file-name :int)
      -1))

;; NOTE: If destination path does not exist, it is created!
(defun file-copy (src-path dst-path)
  "Copy file from one path to another, dstPath created if it doesn't exist, returns 0 on success"
  (multiple-value-bind (src-file-data src-data-size) (load-file-data src-path)
    ;; Create required paths if they do not exist
    (let ((result (if (directory-exists (get-directory-path dst-path))
                      0                                    ; Already exists
                      (make-directory (get-directory-path dst-path)))))
      (when (= result 0)                                     ; Directory created successfully or already exists
        (when (and src-file-data (> src-data-size 0))
          (setf result (if (save-file-data dst-path src-file-data src-data-size) 0 -1))))
      result)))

;; NOTE: If dst directories do not exists they are created
(defun file-move (src-path dst-path)
  "Move file from one directory to another, dstPath created if it doesn't exist, returns 0 on success"
  (let ((result -1))
    (if (file-exists src-path)
        (progn
          (setf result (file-copy src-path dst-path))
          (when (= result 0)
            ;; Make sure file has been correctly copied before removing
            (if (and (file-exists dst-path) (= (get-file-length src-path) (get-file-length dst-path)))
                (progn
                  (setf result (file-remove src-path))
                  (unless (= result 0)
                    (trace-log-warning "FILEIO: [~a] Failed to remove source file after copy" src-path)))
                (trace-log-warning "FILEIO: [~a] Failed to copy file to [~a]" src-path dst-path))))
        (trace-log-warning "FILEIO: [~a] Source file does not exist" src-path))
    result))

(defun file-text-replace (file-name search replacement)
  "Replace text in an existing file, returns 0 on success"
  (if (file-exists file-name)
      (let* ((file-text (load-file-text file-name))
             (file-text-updated (text-replace file-text search replacement)))
        (if (save-file-text file-name file-text-updated) 0 -1))
      -1))

(defun file-text-find-index (file-name search)
  "Find text in existing file, returns -1 if index not found or the byte index otherwise"
  (if (file-exists file-name)
      (let* ((file-text (load-file-text file-name))
             (index (and file-text (search search file-text))))
        (if index (length (babel:string-to-octets file-text :end index :encoding :utf-8)) -1))
      -1))

;;; Compression and Encoding (raylib rcore.c)

(defun compress-data (data data-size)
  "Compress data (DEFLATE algorithm), returns the compressed data and its size"
  ;; Compression level 8, same as stbiw
  (let ((comp-data (sdeflate data data-size 8)))
    (trace-log +log-info+ "SYSTEM: Compress data: Original size: ~d -> Comp. size: ~d" data-size (length comp-data))
    (values comp-data (length comp-data))))

(defun decompress-data (comp-data comp-data-size)
  "Decompress data (DEFLATE algorithm), returns the data and its size"
  (handler-case
      (let ((data (chipz:decompress nil 'chipz:deflate
                                    (subseq (coerce comp-data '(simple-array (unsigned-byte 8) (*))) 0 comp-data-size))))
        (trace-log-info "SYSTEM: Decompress data: Comp. size: ~d -> Original size: ~d" comp-data-size (length data))
        (values data (length data)))
    (error (e)
      (trace-log-warning "SYSTEM: Failed to decompress data: ~a" e)
      (values nil 0))))

;; NOTE: Returns the encoded string and the output size (raylib includes the NULL terminator in it)
(defun encode-data-base64 (data data-size)
  "Encode data to Base64 string"
  (let* ((table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (padded-size (* 3 (ceiling data-size 3)))
         (padding (- padded-size data-size))
         (encoded (make-string (* 4 (floor padded-size 3)))))
    (loop for i from 0 below data-size by 3
          for out from 0 by 4
          do (let ((pack (logior (ash (aref data i) 16)
                                 (ash (if (< (+ i 1) data-size) (aref data (+ i 1)) 0) 8)
                                 (if (< (+ i 2) data-size) (aref data (+ i 2)) 0))))
               (setf (char encoded out) (char table (ldb (byte 6 18) pack))
                     (char encoded (+ out 1)) (char table (ldb (byte 6 12) pack))
                     (char encoded (+ out 2)) (char table (ldb (byte 6 6) pack))
                     (char encoded (+ out 3)) (char table (ldb (byte 6 0) pack)))))
    ;; Add required padding bytes
    (dotimes (p padding) (setf (char encoded (- (length encoded) p 1)) #\=))
    (values encoded (1+ (length encoded)))))

;; NOTE: Returns the decoded data and its size
(defun decode-data-base64 (text)
  "Decode Base64 string (expected NULL terminated)"
  (when (null text) (return-from decode-data-base64 (values nil 0)))
  (flet ((sixtet (ch)
           (let ((p (position ch "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")))
             (or p 0))))
    (let* ((data-size (length text))
           (padding (loop for k downfrom (1- data-size) to 0 while (char= (char text k) #\=) count t))
           (estimated-output-size (- (* 3 (floor data-size 4)) padding))
           (max-output-size (* 3 (floor data-size 4)))
           (decoded (make-array max-output-size :element-type '(unsigned-byte 8) :initial-element 0))
           (output-count 0))
      (loop for i from 0 below data-size by 4
            do (when (>= (+ i 2) data-size)
                 (trace-log-warning "BASE64: Decoding error: Input data size is not valid")
                 (loop-finish))
               (let ((pack (logior (ash (sixtet (char text i)) 18)
                                   (ash (sixtet (char text (+ i 1))) 12)
                                   (ash (if (and (< (+ i 2) data-size) (char/= (char text (+ i 2)) #\=))
                                            (sixtet (char text (+ i 2))) 0)
                                        6)
                                   (if (and (< (+ i 3) data-size) (char/= (char text (+ i 3)) #\=))
                                       (sixtet (char text (+ i 3))) 0))))
                 (when (> (+ output-count 3) max-output-size)
                   (trace-log-warning "BASE64: Decoding error: Output data size is too small")
                   (loop-finish))
                 (setf (aref decoded output-count) (ldb (byte 8 16) pack)
                       (aref decoded (+ output-count 1)) (ldb (byte 8 8) pack)
                       (aref decoded (+ output-count 2)) (ldb (byte 8 0) pack))
                 (incf output-count 3)))
      (values (subseq decoded 0 (max 0 estimated-output-size)) estimated-output-size))))

;;; Logging System (moved from logging.lisp)
;;; This provides comprehensive logging functionality compatible with raylib

;;; Global logging state
(defvar *trace-log-level* +log-info+ "Current trace log level")
(defvar *trace-log-callback* nil "Custom trace log callback function")
(defvar *log-output-stream* nil "Output stream for logging, NIL for the current *standard-output*")
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
  (setf *trace-log-level* (clamp (%enum-value log-level '("LOG-")) +log-all+ +log-none+)))

(defun get-trace-log-level ()
  "Get the current trace log level"
  *trace-log-level*)

(defun set-trace-log-callback (callback)
  "Set custom trace log callback function
   Callback should accept: (level message)"
  (setf *trace-log-callback* callback))

(defun trace-log (log-level message &rest args)
  "Log a message with specified level"
  (setf log-level (%enum-value log-level '("LOG-")))
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
    (let ((stream (or *log-output-stream* *standard-output*)))
      (format stream "~a~%" formatted-line)
      (force-output stream))
    
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
  (setf *log-output-stream* nil)          ; NIL: log to the current *standard-output*
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
