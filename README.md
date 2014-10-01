Divination
==========

Encode images using HTML &lt;div&gt;s, because why not?<sup>*</sup>

## API

```racket
(provide/contract
 [divinate (->* [;; path to PNG, JPEG, GIF, or BMP image file
                 path-string?
                 ;; identifier for the outputted image
                 valid-css-identifier?]
                [;; pixelates the image with a block size > 1
                 #:block-size exact-nonnegative-integer?
                 ;; colours can represented as CSS classes, inline CSS, or efficient mix
                 #:colour-table (or/c 'all 'none 'efficient)
                 ;; image can be compressed with run length encoding
                 #:compress? boolean?]
                 
                 ;; (values CSS HTML)
                (values string? string?))]
 [divinate-to-file (->* [path-string?
                         valid-css-identifier?]
                        [#:block-size exact-nonnegative-integer?
                         #:colour-table (or/c 'all 'none 'efficient)
                         #:compress? boolean?
                         ;; by default, will not overwrite existing file
                         #:clobber? boolean?
                         ;; filename will default to identifier
                         #:filename string?]
                        void?)])
```

## Installing

```
$ git clone https://github.com/lyons/divination.git
$ cd divination
$ raco pkg install
```

## Usage

```racket
(require divination)
(divinate-to-file "./test.jpg"
                  "test-image"
                  #:block-size 5)
```

<sup>*</sup>There are so many reasons why not
