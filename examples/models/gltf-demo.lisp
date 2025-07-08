;;;; GLTF Loader Demonstration
;;;; This demo shows how to use the GLTF loader system in cl-raylib

(require 'cl-raylib)
(in-package :cl-raylib)

(defun gltf-demo ()
  "Demonstrate the GLTF loader capabilities"
  (format t "~%=== Pure-Raylib GLTF Loader Demo ===~%~%")
  
  ;; Show JSON library detection
  (format t "JSON Library Detection:~%")
  (handler-case
    (let ((library (detect-json-library)))
      (format t "  Detected library: ~a~%" library)
      (format t "  Available libraries:~%")
      (format t "    jsown: ~a~%" (if (find-package :jsown) "Available" "Not loaded"))
      (format t "    yason: ~a~%" (if (find-package :yason) "Available" "Not loaded"))
      (format t "    cl-json: ~a~%" (if (find-package :cl-json) "Available" "Not loaded")))
    (error (e)
      (format t "  Error: ~a~%" e)
      (format t "  Please install a JSON library: (ql:quickload :jsown)~%")))
  
  (format t "~%"))

(defun test-gltf-parsing ()
  "Test GLTF JSON parsing with sample data"
  (format t "=== Testing GLTF JSON Parsing ===~%~%")
  
  ;; Create a simple GLTF JSON for testing
  (let ((sample-gltf-json "{
    \"asset\": {
      \"version\": \"2.0\",
      \"generator\": \"cl-raylib demo\",
      \"copyright\": \"Test model\"
    },
    \"scene\": 0,
    \"scenes\": [
      {
        \"name\": \"Test Scene\",
        \"nodes\": [0]
      }
    ],
    \"nodes\": [
      {
        \"name\": \"Test Node\",
        \"mesh\": 0,
        \"translation\": [0.0, 0.0, 0.0],
        \"rotation\": [0.0, 0.0, 0.0, 1.0],
        \"scale\": [1.0, 1.0, 1.0]
      }
    ],
    \"meshes\": [
      {
        \"name\": \"Test Mesh\",
        \"primitives\": [
          {
            \"attributes\": {
              \"POSITION\": 0,
              \"NORMAL\": 1,
              \"TEXCOORD_0\": 2
            },
            \"indices\": 3,
            \"material\": 0,
            \"mode\": 4
          }
        ]
      }
    ],
    \"materials\": [
      {
        \"name\": \"Test Material\",
        \"pbrMetallicRoughness\": {
          \"baseColorFactor\": [1.0, 1.0, 1.0, 1.0],
          \"metallicFactor\": 0.0,
          \"roughnessFactor\": 1.0
        },
        \"alphaMode\": \"OPAQUE\",
        \"doubleSided\": false
      }
    ],
    \"buffers\": [
      {
        \"byteLength\": 1024,
        \"uri\": \"test.bin\"
      }
    ],
    \"bufferViews\": [
      {
        \"buffer\": 0,
        \"byteOffset\": 0,
        \"byteLength\": 288,
        \"target\": 34962
      },
      {
        \"buffer\": 0,
        \"byteOffset\": 288,
        \"byteLength\": 288,
        \"target\": 34962
      },
      {
        \"buffer\": 0,
        \"byteOffset\": 576,
        \"byteLength\": 192,
        \"target\": 34962
      },
      {
        \"buffer\": 0,
        \"byteOffset\": 768,
        \"byteLength\": 36,
        \"target\": 34963
      }
    ],
    \"accessors\": [
      {
        \"bufferView\": 0,
        \"componentType\": 5126,
        \"count\": 24,
        \"type\": \"VEC3\",
        \"min\": [-1.0, -1.0, -1.0],
        \"max\": [1.0, 1.0, 1.0]
      },
      {
        \"bufferView\": 1,
        \"componentType\": 5126,
        \"count\": 24,
        \"type\": \"VEC3\"
      },
      {
        \"bufferView\": 2,
        \"componentType\": 5126,
        \"count\": 24,
        \"type\": \"VEC2\"
      },
      {
        \"bufferView\": 3,
        \"componentType\": 5123,
        \"count\": 36,
        \"type\": \"SCALAR\"
      }
    ]
  }"))
    
    (handler-case
      (let ((gltf-data (parse-gltf-json sample-gltf-json "/test/")))
        (if gltf-data
          (progn
            (format t "GLTF parsing successful!~%")
            (format t "~a~%~%" (get-gltf-info gltf-data))
            
            ;; Show detailed information
            (format t "Asset Information:~%")
            (let ((asset (gltf-data-asset gltf-data)))
              (when asset
                (format t "  Version: ~a~%" (gltf-asset-version asset))
                (format t "  Generator: ~a~%" (gltf-asset-generator asset))
                (format t "  Copyright: ~a~%" (gltf-asset-copyright asset))))
            
            (format t "~%Scenes:~%")
            (loop for i from 0 
                  for scene in (gltf-data-scenes gltf-data) do
              (format t "  Scene ~d: ~a (nodes: ~{~d~^, ~})~%" 
                      i (gltf-scene-name scene) (gltf-scene-nodes scene)))
            
            (format t "~%Nodes:~%")
            (loop for i from 0 
                  for node in (gltf-data-nodes gltf-data) do
              (format t "  Node ~d: ~a~%" i (gltf-node-name node))
              (format t "    Mesh: ~d~%" (gltf-node-mesh node))
              (format t "    Translation: ~a~%" (gltf-node-translation node))
              (format t "    Rotation: ~a~%" (gltf-node-rotation node))
              (format t "    Scale: ~a~%" (gltf-node-scale node)))
            
            (format t "~%Meshes:~%")
            (loop for i from 0 
                  for mesh in (gltf-data-meshes gltf-data) do
              (format t "  Mesh ~d: ~a (~d primitives)~%" 
                      i (gltf-mesh-name mesh) (length (gltf-mesh-primitives mesh))))
            
            (format t "~%Materials:~%")
            (loop for i from 0 
                  for material in (gltf-data-materials gltf-data) do
              (format t "  Material ~d: ~a~%" i (gltf-material-name material))
              (format t "    Alpha Mode: ~a~%" (gltf-material-alpha-mode material))
              (format t "    Double Sided: ~a~%" (gltf-material-double-sided material))))
          
          (format t "GLTF parsing failed~%")))
      
      (error (e)
        (format t "Error parsing GLTF: ~a~%" e))))
  
  (format t "~%"))

(defun test-gltf-constants ()
  "Test GLTF constants and utilities"
  (format t "=== Testing GLTF Constants and Utilities ===~%~%")
  
  (format t "Component Types:~%")
  (format t "  BYTE: ~d (size: ~d)~%" +gltf-byte+ (get-component-size +gltf-byte+))
  (format t "  UNSIGNED_BYTE: ~d (size: ~d)~%" +gltf-unsigned-byte+ (get-component-size +gltf-unsigned-byte+))
  (format t "  SHORT: ~d (size: ~d)~%" +gltf-short+ (get-component-size +gltf-short+))
  (format t "  UNSIGNED_SHORT: ~d (size: ~d)~%" +gltf-unsigned-short+ (get-component-size +gltf-unsigned-short+))
  (format t "  UNSIGNED_INT: ~d (size: ~d)~%" +gltf-unsigned-int+ (get-component-size +gltf-unsigned-int+))
  (format t "  FLOAT: ~d (size: ~d)~%" +gltf-float+ (get-component-size +gltf-float+))
  
  (format t "~%Accessor Types:~%")
  (dolist (type '("SCALAR" "VEC2" "VEC3" "VEC4" "MAT2" "MAT3" "MAT4"))
    (format t "  ~a: ~d components~%" type (get-type-components type)))
  
  (format t "~%Primitive Modes:~%")
  (format t "  POINTS: ~d~%" +gltf-points+)
  (format t "  LINES: ~d~%" +gltf-lines+)
  (format t "  LINE_LOOP: ~d~%" +gltf-line-loop+)
  (format t "  LINE_STRIP: ~d~%" +gltf-line-strip+)
  (format t "  TRIANGLES: ~d~%" +gltf-triangles+)
  (format t "  TRIANGLE_STRIP: ~d~%" +gltf-triangle-strip+)
  (format t "  TRIANGLE_FAN: ~d~%" +gltf-triangle-fan+)
  
  (format t "~%Buffer Targets:~%")
  (format t "  ARRAY_BUFFER: ~d~%" +gltf-array-buffer+)
  (format t "  ELEMENT_ARRAY_BUFFER: ~d~%" +gltf-element-array-buffer+)
  
  (format t "~%"))

(defun test-data-uri-handling ()
  "Test data URI handling"
  (format t "=== Testing Data URI Handling ===~%~%")
  
  ;; Test data URI detection
  (let ((test-uris '("data:application/octet-stream;base64,SGVsbG8gV29ybGQ="
                     "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8/5+hHgAHggJ/PchI7wAAAABJRU5ErkJggg=="
                     "textures/diffuse.png"
                     "models/cube.bin")))
    
    (format t "Data URI Detection:~%")
    (dolist (uri test-uris)
      (format t "  ~a~%    -> ~a~%" 
              (if (> (length uri) 50)
                (format nil "~a..." (subseq uri 0 50))
                uri)
              (if (string-starts-with uri "data:")
                "Data URI"
                "External file"))))
  
  (format t "~%"))

(defun demo-gltf-model-conversion ()
  "Demonstrate GLTF to model conversion process"
  (format t "=== GLTF Model Conversion Demo ===~%~%")
  
  (format t "Model Conversion Process:~%")
  (format t "1. Parse GLTF JSON structure~%")
  (format t "2. Load external buffers and images~%")
  (format t "3. Extract mesh data from accessors~%")
  (format t "4. Convert primitives to cl-raylib meshes~%")
  (format t "5. Convert materials~%")
  (format t "6. Create final model structure~%")
  
  (format t "~%Supported Features:~%")
  (format t "  ✓ GLTF 2.0 specification~%")
  (format t "  ✓ Multiple JSON libraries (jsown, yason, cl-json)~%")
  (format t "  ✓ External buffer loading~%")
  (format t "  ✓ Data URI support (base64)~%")
  (format t "  ✓ Mesh primitive extraction~%")
  (format t "  ✓ Material conversion~%")
  (format t "  ✓ Scene graph support~%")
  (format t "  ✓ Validation system~%")
  
  (format t "~%Limitations:~%")
  (format t "  • Simplified base64 decoder (use proper library for production)~%")
  (format t "  • Basic IEEE 754 float reading~%")
  (format t "  • Limited animation support~%")
  (format t "  • Simplified PBR material conversion~%")
  
  (format t "~%"))

(defun test-gltf-file-loading ()
  "Test GLTF file loading with sample files"
  (format t "=== Testing GLTF File Loading ===~%~%")
  
  ;; Create a minimal GLTF file for testing
  (let ((test-filename "/tmp/test-model.gltf")
        (minimal-gltf "{
  \"asset\": {
    \"version\": \"2.0\",
    \"generator\": \"cl-raylib test\"
  },
  \"scene\": 0,
  \"scenes\": [
    {
      \"name\": \"Scene\",
      \"nodes\": [0]
    }
  ],
  \"nodes\": [
    {
      \"name\": \"Cube\",
      \"mesh\": 0
    }
  ],
  \"meshes\": [
    {
      \"name\": \"Cube\",
      \"primitives\": [
        {
          \"attributes\": {
            \"POSITION\": 0
          },
          \"mode\": 4
        }
      ]
    }
  ],
  \"buffers\": [
    {
      \"byteLength\": 72
    }
  ],
  \"bufferViews\": [
    {
      \"buffer\": 0,
      \"byteOffset\": 0,
      \"byteLength\": 72,
      \"target\": 34962
    }
  ],
  \"accessors\": [
    {
      \"bufferView\": 0,
      \"componentType\": 5126,
      \"count\": 6,
      \"type\": \"VEC3\",
      \"min\": [-1.0, -1.0, -1.0],
      \"max\": [1.0, 1.0, 1.0]
    }
  ]
}"))
    
    ;; Write test file
    (save-file-text test-filename minimal-gltf)
    (format t "Created test GLTF file: ~a~%" test-filename)
    
    ;; Test loading
    (handler-case
      (let ((gltf-data (load-gltf-file test-filename)))
        (if gltf-data
          (progn
            (format t "GLTF file loaded successfully!~%")
            (format t "~a~%" (get-gltf-info gltf-data))
            
            ;; Test model conversion
            (format t "~%Testing model conversion...~%")
            (let ((model (gltf-to-model gltf-data)))
              (if model
                (format t "Model conversion successful!~%")
                (format t "Model conversion failed~%"))))
          
          (format t "GLTF file loading failed~%")))
      
      (error (e)
        (format t "Error loading GLTF file: ~a~%" e)))
    
    ;; Cleanup
    (when (uiop:file-exists-p test-filename)
      (delete-file-safe test-filename)
      (format t "Cleaned up test file~%")))
  
  (format t "~%"))

(defun show-gltf-data-structures ()
  "Show GLTF data structure information"
  (format t "=== GLTF Data Structures ===~%~%")
  
  (format t "Main Structures:~%")
  (format t "  gltf-data - Main container for all GLTF data~%")
  (format t "  gltf-asset - Asset metadata (version, generator, etc.)~%")
  (format t "  gltf-scene - Scene with node references~%")
  (format t "  gltf-node - Scene graph node with transform~%")
  (format t "  gltf-mesh - Mesh with primitives~%")
  (format t "  gltf-primitive - Renderable geometry~%")
  (format t "  gltf-material - PBR material definition~%")
  
  (format t "~%Data Access Structures:~%")
  (format t "  gltf-buffer - Raw binary data~%")
  (format t "  gltf-buffer-view - View into buffer~%")
  (format t "  gltf-accessor - Typed access to buffer data~%")
  
  (format t "~%Resource Structures:~%")
  (format t "  gltf-texture - Texture reference~%")
  (format t "  gltf-image - Image data or URI~%")
  
  (format t "~%Configuration Variables:~%")
  (format t "  *gltf-json-library* - JSON library preference~%")
  (format t "  *gltf-load-buffers* - Auto-load external buffers~%")
  (format t "  *gltf-load-images* - Auto-load external images~%")
  (format t "  *gltf-validate-data* - Validate after loading~%")
  
  (format t "~%"))

(defun run-complete-gltf-demo ()
  "Run complete GLTF loader demonstration"
  (gltf-demo)
  (test-gltf-parsing)
  (test-gltf-constants)
  (test-data-uri-handling)
  (demo-gltf-model-conversion)
  (test-gltf-file-loading)
  (show-gltf-data-structures)
  
  (format t "~%=== Complete GLTF Demo Finished ===~%")
  (format t "Note: Full functionality requires installing a JSON library:~%")
  (format t "  - (ql:quickload :jsown) - Fast JSON parsing~%")
  (format t "  - (ql:quickload :yason) - Full-featured JSON library~%")
  (format t "  - (ql:quickload :cl-json) - CLOS integration~%")
  (format t "~%For production use, also consider:~%")
  (format t "  - A proper base64 library for data URI support~%")
  (format t "  - IEEE 754 binary parsing library for accurate float reading~%"))

;; Run demonstration when file is loaded
(eval-when (:load-toplevel :execute)
  (format t "~%Loading GLTF demo...~%")
  (format t "Run (cl-raylib:run-complete-gltf-demo) to see the demonstration~%"))