#lang racket

;; Many things in this progam are not performed in the most efficient manner.
;; This seems oddly appropriate given what it does.

; (-> String Boolean)
; doesn't include css-identifiers that start with - or _, because they have weird rules
(define (valid-css-identifier? x)
  (regexp-match? (pregexp "^[a-zA-Z]+[a-zA-Z0-9_-]*$") x))

(provide/contract
 [divinate (->* [path-string? valid-css-identifier?]
                [#:block-size exact-nonnegative-integer?
                 #:colour-table (or/c 'all 'none 'efficient)
                 #:compress? boolean?]
                (values string? string?))]
 [divinate-to-file (->* [path-string? valid-css-identifier?]
                        [#:block-size exact-nonnegative-integer?
                         #:colour-table (or/c 'all 'none 'efficient)
                         #:compress? boolean?
                         #:clobber? boolean?
                         #:filename string?]
                        void?)])

(require racket/draw
         xml)

(define divinate
  (λ (path
      title
      #:block-size [block-size 1]
      #:colour-table [colours 'efficient]
      #:compress? [compress? #t])
    (define image
      (let ([img (load-image path block-size)])
        (if compress?
            (compress-image img)
            img)))
    (define colours-dict
      (cond
        [(eq? colours 'efficient) (make-colour-dict (count-colours image))]
        [(eq? colours 'all) (make-colour-dict (count-colours image) #:filter-level 0)]
        [(eq? colours 'none) (make-hash)]))
    (define run-lengths-dict
      (if compress?
          (make-run-length-dict (count-run-lengths image))
          (make-hash)))
    (define css
      (string-append (image-css image title) "\n"
                     (colours-css colours-dict)
                     (if (hash-empty? colours-dict) "" "\n")
                     (run-lengths-css run-lengths-dict (base-percent image) title)
                     (if (hash-empty? run-lengths-dict) "" "\n")))
    (define html
      (image->html image title #:colours colours-dict #:run-lengths run-lengths-dict))
    
    (values css html)))

(define divinate-to-file
  (λ (path
      title
      #:block-size [block-size 1]
      #:colour-table [colours 'efficient]
      #:compress? [compress? #t]
      #:clobber? [clobber? #f]
      #:filename [filename title])
    (define-values (css html)
      (divinate path
                title
                #:block-size block-size
                #:colour-table 'efficient
                #:compress? compress?))
    (define page
      (string-append "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<title>"
                     title
                     "</title>\n<style type=\"text/css\">\n"
                     css
                     "</style>\n</head>\n<body>\n"
                     html
                     "\n</body>\n</html>\n"))
    (display-to-file page
                     (string-append "./" filename ".html")
                     #:exists (if clobber? 'replace 'error))))

;;----------------------------------------------------------------------------------------------------

; internal representation of image
; image data stored as list of list of [hex colour code or (run-length hex-colour-code)]
; scale represents size of box-blur used on image
(struct Image
  (data    ; (Listof (Listof (U String (List Positive-Integer String))))
   width   ; Positive-Integer
   height  ; Positive-Integer
   scale)) ; Positive-Integer

; (-> Positive-Integer String)
; better hope n isn't greater than 0xFFFFFF
(define (integer->hex-colour n)
  (format (~a (~r n #:base 16) #:width 6 #:align 'right #:pad-string "0")))

; (-> Path Positive-Integer Image)
; loads an image, if block-size is > 1 performs a box blur to create a pixelated effect
(define load-image
  (λ (path
      block-size)
    (define image (make-object bitmap% 1 1))
    (send image load-file path 'unknown)
    
    (define-values (blocks-x offset-x)
      (let* ([width (send image get-width)]
             [excess (remainder width block-size)])
        (values (quotient width block-size) (quotient excess 2))))
    (define-values (blocks-y offset-y)
      (let* ([height (send image get-height)]
             [excess (remainder height block-size)])
        (values (quotient height block-size) (quotient excess 2))))
    
    (define buffer (make-bytes (* 4 block-size block-size)))
    
    (define (read-chunk x y)
      (send image get-argb-pixels
            (+ offset-x (* block-size x)) ; x
            (+ offset-y (* block-size y)) ; y
            block-size ; width
            block-size ; height
            buffer))
    
    (define (read-buffer)
      (define (read-pixel n)
        (let* ([pixel-offset (* 4 n)]
               [r (bytes-ref buffer (+ pixel-offset 1))]
               [g (bytes-ref buffer (+ pixel-offset 2))]
               [b (bytes-ref buffer (+ pixel-offset 3))])
          `(,r ,g ,b)))
      
      (define (box-blur pxls)
        (define avg-colour
          (foldl (λ (pxl acc)
                   (match-let ([`(,r1 ,g1 ,b1) pxl]
                               [`(,r2 ,g2 ,b2) acc])
                     `(,(+ r1 r2) ,(+ g1 g2) ,(+ b1 b2))))
                 '(0 0 0)
                 pxls))
        (map (λ (c) (quotient c (* block-size block-size))) avg-colour))
      
      (box-blur (map read-pixel
                     (range 0 (* block-size block-size)))))
        
    (define processed-pixels
      (let ([coörds (map (λ (y)
                           (map (λ (x) `(,x ,y)) (range 0 blocks-x)))
                         (range 0 blocks-y))])
        (map (λ (row)
               (map (match-lambda
                      [`(,x ,y) (let* ([pxl (begin (read-chunk x y) (read-buffer))]
                                       [val ((match-lambda [`(,r ,g ,b)
                                                            (+ (* r 256 256) (* g 256) b)]) pxl)]
                                       [hex ((compose string-upcase integer->hex-colour)
                                             val)])
                                  hex)])
                    row))
             coörds)))
    (Image processed-pixels blocks-x blocks-y block-size)))

; (-> Image Image)
; run-length encodes image data
(define (compress-image image)
  (define run-length-encode
    (match-lambda
      [`(,h . ,t) (define-values (tail count) (munch t h))
                  (if (> count 1)
                      (cons `(,count ,h) (run-length-encode tail))
                      (cons h (run-length-encode tail)))]
      ['() '()]))
  
  (define (munch ls val)
    (define (munch-h ls n)
      (match ls
        [`(,h . ,t) #:when (equal? h val)
                    (munch-h t (+ n 1))]
        [_ (values ls n)]))
    (munch-h ls 1))
  
  (define compressed-data (map run-length-encode (Image-data image)))
  (Image compressed-data
         (Image-width image)
         (Image-height image)
         (Image-scale image)))

; (->* [Image] [(HashTable Integer Integer)] (Hashtable Integer Integer))
; builds a table associating every run length occuring in the image with its frequency
(define count-run-lengths
  (λ (image
      [hash (make-hash)])
    (for ([i (Image-data image)])
      (for ([j i])
        (match j
          [`(,n ,_) (hash-update! hash
                                  n
                                  (λ (x) (+ x 1))
                                  (λ () 0))]
          [_ '()])))
    hash))

; (-> (HashTable Integer Integer) (HashTable Integer String))
; builds a table associating every run length with a css class, most frequent lengths having the
; shortest class names
(define make-run-length-dict
  (λ (count-dict)
    (define hash (make-hash))
    (define ls (hash->list count-dict))
    (define ls-sorted (sort ls (λ (x y) (> (cdr x) (cdr y)))))
    (let loop [(runs ls-sorted) (n 0)]
      (cond
        [(null? runs) hash]
        [else (hash-set! hash
                         (car (car runs))
                         (string-append "r" (number->string n)))
              (loop (cdr runs) (+ n 1))]))))

; (->* [Image] [(HashTable String Integer)] (Hashtable String Integer))
; builds a table associating every colour occuring in the image with its frequency
(define count-colours
  (λ (image
      [hash (make-hash)])
    (for ([i (Image-data image)])
      (for ([j i])
        (hash-update! hash
                      (match j [`(,_ ,c) c] [c c])
                      (λ (n) (+ n 1))
                      (λ () 0))))
    hash))

; (->* [(HashTable String Integer)] [Natural] (HashTable String String))
; builds a table associating every colour with a css class, most frequent colurs having the shortest
; class names
; default filter level excludes singleton colours, as it is more space efficient to inline them rather
; than defining a class
(define make-colour-dict
  (λ (count-dict
      #:filter-level [filter-level 1])
    (define hash (make-hash))
    (define ls (hash->list count-dict))
    (define ls-sorted (sort ls (λ (x y) (> (cdr x) (cdr y)))))
    (define ls-filtered (if (> filter-level 0)
                            (filter (λ (n) (> (cdr n) filter-level)) ls-sorted)
                            ls-sorted))
    (let loop [(colours ls-filtered) (n 0)]
      (cond
        [(null? colours) hash]
        [else (hash-set! hash
                         (car (car colours))
                         (string-append "c" (number->string n)))
              (loop (cdr colours) (+ n 1))]))))

; (-> Image Positive-Integer)
(define (scaled-width image)
  (* (Image-scale image) (Image-width image)))
(define (scaled-height image)
  (* (Image-scale image) (Image-height image)))

; (-> Image Inexact-Real)
(define (base-percent image)
  (exact->inexact (/ 100 (Image-width image))))
(define (height-percent image)
  (exact->inexact (/ 100 (Image-height image))))

; (-> Image String String)
; outputs the css for the image, with title as css id
(define (image-css image title)
  (string-join
   `(,(string-append "#" title)
     "{"
     "  margin: auto;"
     "  padding: 0px;"
     "  font-size: 1px;"
     "  line-height: 0px;"
     ,(string-append "  /* original image width: "
                     (number->string (scaled-width image))
                     "px, scale: integer multiples of "
                     (number->string (Image-width image))
                     "px */")
     ,(string-append "  width: " (number->string (scaled-width image)) "px;")
     ,(string-append "  height: " (number->string (scaled-height image)) "px;")
     "}"
     ,(string-append "#" title " > div")
     "{"
     "  display: inline-block;"
     "  margin: 0px;"
     "  padding: 0px;"
     ,(string-append "  width: " (number->string (base-percent image)) "%;")
     ,(string-append "  height: " (number->string (height-percent image)) "%;")
     "}")
   "\n"))

; (-> (HashDict String String) String)
; outputs the css classes for the colour dictionary
(define (colours-css colour-dict)
  (define ls
    (sort (hash->list colour-dict)
          (λ (a b) (string<? (cdr a) (cdr b)))))
  
  (define css-ls
    (map (λ (x)
           (match x [`(,colour . ,class) (string-append "." class "{background:#" colour "}")]))
         ls))
  
  (string-join css-ls "\n"))

; (-> (HashTable Integer String) Inexact-Real String String)
; outputs the css for the run lengths table
(define (run-lengths-css run-length-dict base-pct title)
  (define ls
    (sort (hash->list run-length-dict)
          (λ (a b) (string<? (cdr a) (cdr b)))))
  
  (define css-ls
    (map (λ (x)
           (match x
             [`(,run . ,class)
              (string-append "#" title " > ." class
                             "{width:" (number->string (* run base-pct)) "%}")]))
         ls))
  
  (string-join css-ls "\n"))

; (->* [Image String]
;      [#:colours (HashTable String String) #:run-lengths (HashTable String String)]
;      String)
; outputs the image as a series of divs
(define image->html 
  (λ (image
      title
      #:colours [colour-dict (make-hash)]
      #:run-lengths [run-length-dict (make-hash)])
  (define (pixel->xexpr px)
    (define colour-class (hash-ref colour-dict
                                   (match px [`(,_ ,c) c] [c c])
                                   (λ () #f)))
    (define run-length-class (hash-ref run-length-dict
                                       (match px [`(,r ,_) r] [_ #f])
                                       (λ () #f)))
    (define class-xexpr
      (cond
        [(and colour-class run-length-class)
         `(class ,(string-append colour-class " " run-length-class))]
        [colour-class `(class ,colour-class)]
        [run-length-class `(class ,run-length-class)]
        [else #f]))
    (define style-xexpr
      (cond
        [(not colour-class) `(style ,(string-append "background:#"
                                                    (match px [`(,_ ,c) c] [c c])))]
        [else #f]))
    (cond
      [(and class-xexpr style-xexpr) `(div (,class-xexpr ,style-xexpr))]
      [class-xexpr `(div (,class-xexpr))]
      [style-xexpr `(div (,style-xexpr))]))
  
  (define (row->xexpr row)
    `(,@(map pixel->xexpr row) (br)))
  
  (define image-xexpr
    `(div ([id ,title])
          ,@(apply append (map row->xexpr (Image-data image)))))
  
  (xexpr->string image-xexpr)))