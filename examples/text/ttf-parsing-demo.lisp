;;;; TTF/OTF Font Parsing Demo
;;;; Comprehensive demonstration of TrueType font parsing capabilities

(ql:quickload :cl-raylib)
(use-package :cl-raylib)

(defun demo-ttf-format-detection ()
  "Demonstrate font format detection"
  (format t "~%=== TTF Format Detection Demo ===~%")
  
  (let ((test-files '("test.ttf" "font.otf" "collection.ttc" "bitmap.fnt" "unknown.xyz")))
    (format t "Testing format detection for various file extensions:~%")
    (format t "Filename        | Detected Format~%")
    (format t "----------------|----------------~%")
    
    (dolist (filename test-files)
      (let ((format (detect-font-format filename)))
        (format t "~15a | ~a~%" filename format))))
  
  (format t "Format detection demo completed.~%"))

(defun demo-ttf-binary-parsing ()
  "Demonstrate binary data parsing functions"
  (format t "~%=== TTF Binary Parsing Demo ===~%")
  
  ;; Create test binary data
  (let ((test-data (make-array 20 :element-type '(unsigned-byte 8)
                              :initial-contents '(#x00 #x01 #x02 #x03  ; uint32: 66051
                                                 #x12 #x34              ; uint16: 4660
                                                 #xFF #xFF              ; int16: -1
                                                 #x48 #x65 #x6C #x6C    ; tag: "Hell"
                                                 #x6F #x00 #x00 #x00    ; "o" + padding
                                                 #x80 #x00 #x7F #xFF)))) ; int16: -32768, 32767
    
    (format t "Testing binary parsing functions:~%")
    (format t "Function         | Offset | Result~%")
    (format t "-----------------|--------|--------~%")
    (format t "read-uint32-be   |   0    | ~d~%" (read-uint32-be test-data 0))
    (format t "read-uint16-be   |   4    | ~d~%" (read-uint16-be test-data 4))
    (format t "read-int16-be    |   6    | ~d~%" (read-int16-be test-data 6))
    (format t "read-tag         |   8    | ~a~%" (read-tag test-data 8))
    (format t "read-int16-be    |  16    | ~d~%" (read-int16-be test-data 16))
    (format t "read-int16-be    |  18    | ~d~%" (read-int16-be test-data 18)))
  
  (format t "Binary parsing demo completed.~%"))

(defun demo-ttf-header-parsing ()
  "Demonstrate TTF header parsing"
  (format t "~%=== TTF Header Parsing Demo ===~%")
  
  ;; Create mock TTF header data
  (let ((header-data (make-array 12 :element-type '(unsigned-byte 8)
                                :initial-contents '(#x00 #x01 #x00 #x00  ; scalar type
                                                   #x00 #x0C              ; 12 tables
                                                   #x00 #x80              ; search range
                                                   #x00 #x03              ; entry selector
                                                   #x00 #x20))))          ; range shift
    
    (let ((header (parse-ttf-header header-data)))
      (format t "Parsed TTF Header:~%")
      (format t "- Scalar Type: ~d~%" (ttf-header-scalar-type header))
      (format t "- Number of Tables: ~d~%" (ttf-header-num-tables header))
      (format t "- Search Range: ~d~%" (ttf-header-search-range header))
      (format t "- Entry Selector: ~d~%" (ttf-header-entry-selector header))
      (format t "- Range Shift: ~d~%" (ttf-header-range-shift header))))
  
  (format t "Header parsing demo completed.~%"))

(defun demo-ttf-table-structures ()
  "Demonstrate TTF table structure creation"
  (format t "~%=== TTF Table Structures Demo ===~%")
  
  ;; Create sample table entries
  (let ((head-table (make-ttf-table-entry :tag "head"
                                         :checksum #x12345678
                                         :offset 1024
                                         :length 54))
        (hhea-table (make-ttf-table-entry :tag "hhea"
                                         :checksum #x87654321
                                         :offset 2048
                                         :length 36))
        (cmap-table (make-ttf-table-entry :tag "cmap"
                                         :checksum #xABCDEF01
                                         :offset 4096
                                         :length 1024)))
    
    (format t "Sample TTF Table Entries:~%")
    (format t "Tag  | Checksum   | Offset | Length~%")
    (format t "-----|------------|--------|-------~%")
    (format t "~4a | ~10X |  ~5d |  ~5d~%" 
            (ttf-table-entry-tag head-table)
            (ttf-table-entry-checksum head-table)
            (ttf-table-entry-offset head-table)
            (ttf-table-entry-length head-table))
    (format t "~4a | ~10X |  ~5d |  ~5d~%" 
            (ttf-table-entry-tag hhea-table)
            (ttf-table-entry-checksum hhea-table)
            (ttf-table-entry-offset hhea-table)
            (ttf-table-entry-length hhea-table))
    (format t "~4a | ~10X |  ~5d |  ~5d~%" 
            (ttf-table-entry-tag cmap-table)
            (ttf-table-entry-checksum cmap-table)
            (ttf-table-entry-offset cmap-table)
            (ttf-table-entry-length cmap-table)))
  
  (format t "Table structures demo completed.~%"))

(defun demo-ttf-font-data-creation ()
  "Demonstrate TTF font data structure creation and manipulation"
  (format t "~%=== TTF Font Data Demo ===~%")
  
  ;; Create font data structure
  (let ((font-data (make-ttf-font-data :format :ttf
                                      :units-per-em 2048
                                      :ascender 1600
                                      :descender -400
                                      :line-gap 0)))
    
    (format t "Created TTF Font Data:~%")
    (format t "- Format: ~a~%" (ttf-font-data-format font-data))
    (format t "- Units per EM: ~d~%" (ttf-font-data-units-per-em font-data))
    (format t "- Ascender: ~d~%" (ttf-font-data-ascender font-data))
    (format t "- Descender: ~d~%" (ttf-font-data-descender font-data))
    (format t "- Line Gap: ~d~%" (ttf-font-data-line-gap font-data))
    (format t "- Table Count: ~d~%" (hash-table-count (ttf-font-data-tables font-data)))
    (format t "- Glyph Count: ~d~%" (hash-table-count (ttf-font-data-glyphs font-data)))
    
    ;; Add some sample tables
    (let ((tables (ttf-font-data-tables font-data)))
      (setf (gethash "head" tables) (make-ttf-table-entry :tag "head" :offset 100 :length 54))
      (setf (gethash "hhea" tables) (make-ttf-table-entry :tag "hhea" :offset 200 :length 36))
      (setf (gethash "cmap" tables) (make-ttf-table-entry :tag "cmap" :offset 300 :length 512)))
    
    ;; Generate basic glyphs
    (generate-basic-glyphs font-data)
    
    (format t "~%After adding tables and generating glyphs:~%")
    (format t "- Table Count: ~d~%" (hash-table-count (ttf-font-data-tables font-data)))
    (format t "- Glyph Count: ~d~%" (hash-table-count (ttf-font-data-glyphs font-data)))
    
    ;; Show some glyph information
    (format t "~%Sample Glyph Information:~%")
    (format t "Char | Value | Advance~%")
    (format t "-----|-------|--------~%")
    (loop for char across "HELLO" do
      (let ((glyph (gethash char (ttf-font-data-glyphs font-data))))
        (when glyph
          (format t " ~c   |  ~3d  |  ~5d~%" 
                  char 
                  (glyph-info-value glyph)
                  (glyph-info-advance-x glyph))))))
  
  (format t "Font data demo completed.~%"))

(defun demo-ttf-glyph-bitmap ()
  "Demonstrate glyph bitmap creation"
  (format t "~%=== TTF Glyph Bitmap Demo ===~%")
  
  ;; Create bitmaps for different characters
  (let ((test-chars "ABC123"))
    (format t "Creating glyph bitmaps for characters: ~a~%" test-chars)
    (format t "Bitmap size: 16x16 pixels~%~%")
    
    (loop for char across test-chars do
      (let ((bitmap (create-simple-glyph-bitmap char 16 16)))
        (format t "Character '~c' bitmap pattern:~%" char)
        (loop for y from 0 below 16 do
          (format t "  ")
          (loop for x from 0 below 16 do
            (let ((pixel (aref bitmap (+ (* y 16) x))))
              (format t "~c" (cond ((= pixel 255) #\#)
                                  ((= pixel 128) #\.)
                                  (t #\Space)))))
          (format t "~%"))
        (format t "~%"))))
  
  (format t "Glyph bitmap demo completed.~%"))

(defun demo-ttf-complete-parsing ()
  "Demonstrate complete TTF parsing workflow"
  (format t "~%=== Complete TTF Parsing Workflow Demo ===~%")
  
  ;; Create a realistic mock TTF file structure
  (let ((mock-ttf-data (create-mock-ttf-data)))
    (format t "Created mock TTF data (~d bytes)~%" (length mock-ttf-data))
    
    ;; Parse the data
    (let ((font-data (parse-truetype-data mock-ttf-data :ttf)))
      (format t "~%Parsing Results:~%")
      (format t "- Format: ~a~%" (ttf-font-data-format font-data))
      (format t "- Tables parsed: ~d~%" (hash-table-count (ttf-font-data-tables font-data)))
      (format t "- Glyphs generated: ~d~%" (hash-table-count (ttf-font-data-glyphs font-data)))
      (format t "- Units per EM: ~d~%" (ttf-font-data-units-per-em font-data))
      
      ;; Test atlas creation
      (let* ((config (create-font-loader-config :base-size 16 :chars "ABCabc123"))
             (atlas (create-font-atlas-from-truetype font-data config)))
        (format t "~%Atlas Creation:~%")
        (format t "- Atlas created successfully~%")
        (format t "- Atlas size: ~dx~d~%" 
                (font-atlas-width atlas) 
                (font-atlas-height atlas))
        
        ;; Create final font
        (let ((font (create-font-from-atlas atlas config)))
          (format t "~%Final Font:~%")
          (format t "- Base size: ~d~%" (font-base-size font))
          (format t "- Glyph count: ~d~%" (font-glyph-count font))
          (format t "- Font created successfully~%")))))
  
  (format t "Complete parsing workflow demo completed.~%"))

(defun create-mock-ttf-data ()
  "Create mock TTF file data for testing"
  (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; TTF header
    (setf (aref data 0) #x00 (aref data 1) #x01 (aref data 2) #x00 (aref data 3) #x00) ; scalar type
    (setf (aref data 4) #x00 (aref data 5) #x03) ; 3 tables
    (setf (aref data 6) #x00 (aref data 7) #x30) ; search range
    (setf (aref data 8) #x00 (aref data 9) #x01) ; entry selector
    (setf (aref data 10) #x00 (aref data 11) #x00) ; range shift
    
    ;; Table directory entries
    ;; head table
    (setf (aref data 12) (char-code #\h) (aref data 13) (char-code #\e)
          (aref data 14) (char-code #\a) (aref data 15) (char-code #\d))
    (setf (aref data 20) #x00 (aref data 21) #x00 (aref data 22) #x01 (aref data 23) #x00) ; offset 256
    (setf (aref data 24) #x00 (aref data 25) #x00 (aref data 26) #x00 (aref data 27) #x36) ; length 54
    
    ;; hhea table
    (setf (aref data 28) (char-code #\h) (aref data 29) (char-code #\h)
          (aref data 30) (char-code #\e) (aref data 31) (char-code #\a))
    (setf (aref data 36) #x00 (aref data 37) #x00 (aref data 38) #x02 (aref data 39) #x00) ; offset 512
    (setf (aref data 40) #x00 (aref data 41) #x00 (aref data 42) #x00 (aref data 43) #x24) ; length 36
    
    ;; cmap table
    (setf (aref data 44) (char-code #\c) (aref data 45) (char-code #\m)
          (aref data 46) (char-code #\a) (aref data 47) (char-code #\p))
    (setf (aref data 52) #x00 (aref data 53) #x00 (aref data 54) #x03 (aref data 55) #x00) ; offset 768
    (setf (aref data 56) #x00 (aref data 57) #x00 (aref data 58) #x00 (aref data 59) #x80) ; length 128
    
    ;; Mock head table data at offset 256
    (setf (aref data 256) #x00 (aref data 257) #x01 (aref data 258) #x00 (aref data 259) #x00) ; version
    (setf (aref data 274) #x08 (aref data 275) #x00) ; units per em = 2048
    
    ;; Mock hhea table data at offset 512
    (setf (aref data 512) #x00 (aref data 513) #x01 (aref data 514) #x00 (aref data 515) #x00) ; version
    (setf (aref data 516) #x06 (aref data 517) #x40) ; ascender = 1600
    (setf (aref data 518) #xFE (aref data 519) #x70) ; descender = -400
    
    ;; Mock cmap table data at offset 768
    (setf (aref data 768) #x00 (aref data 769) #x00) ; version
    (setf (aref data 770) #x00 (aref data 771) #x01) ; number of subtables
    
    data))

(defun demo-ttf-system-integration ()
  "Demonstrate TTF system integration with existing font system"
  (format t "~%=== TTF System Integration Demo ===~%")
  
  ;; Show supported formats
  (format t "Supported font formats: ~{~a~^, ~}~%" (get-supported-font-formats))
  (format t "TTF format supported: ~a~%" (is-font-format-supported :truetype))
  (format t "OTF format supported: ~a~%" (is-font-format-supported :opentype))
  
  ;; Test font loading with different paths
  (let ((test-fonts '("test.ttf" "font.otf" "nonexistent.ttf")))
    (format t "~%Testing font loading:~%")
    (format t "Filename        | Result~%")
    (format t "----------------|-------~%")
    
    (dolist (font-file test-fonts)
      (let* ((config (create-font-loader-config :base-size 16))
             (result (handler-case
                       (load-truetype-font font-file config)
                       (error (e) (format nil "Error: ~a" e)))))
        (format t "~15a | ~a~%" 
                font-file 
                (if (font-p result) "Font loaded" result)))))
  
  (format t "~%Format information: ~a~%" (get-font-format-info))
  (format t "System integration demo completed.~%"))

(defun run-all-ttf-demos ()
  "Run all TTF parsing demos in sequence"
  (format t "========================================~%")
  (format t "Pure-Raylib TTF/OTF Parsing Demo Suite~%")
  (format t "========================================~%")
  
  ;; Run all demos
  (demo-ttf-format-detection)
  (demo-ttf-binary-parsing)
  (demo-ttf-header-parsing)
  (demo-ttf-table-structures)
  (demo-ttf-font-data-creation)
  (demo-ttf-glyph-bitmap)
  (demo-ttf-complete-parsing)
  (demo-ttf-system-integration)
  
  (format t "~%========================================~%")
  (format t "All TTF parsing demos completed successfully!~%")
  (format t "~%Key features demonstrated:~%")
  (format t "- Binary data parsing (big-endian integers, tags)~%")
  (format t "- TTF header and table directory parsing~%")
  (format t "- Essential table parsing (head, hhea, cmap)~%")
  (format t "- Glyph generation and bitmap creation~%")
  (format t "- Font atlas creation from TTF data~%")
  (format t "- Complete font loading workflow~%")
  (format t "- System integration with existing font API~%")
  (format t "========================================~%"))

;; Auto-run when loaded
(eval-when (:load-toplevel :execute)
  (format t "TTF Parsing Demo loaded. Run (run-all-ttf-demos) to see all demos.~%"))