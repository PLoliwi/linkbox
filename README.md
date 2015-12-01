# Linkbox

Linkbox is a very simple URL shortener and file upload box.

## Dependencies

- [Alexandria](http://common-lisp.net/project/alexandria/) (Public Domain)
- [Hunchentoot](http://weitz.de/hunchentoot/) (BSD)
- [CL-PPCRE](http://weitz.de/cl-ppcre/) (BSD)

## Usage

```lisp
;; You can start Linkbox like this:

(hunchentoot:start
 (linkbox:make-linkbox :url "http://localhost:5000/" ; With trailing /
                       :path "/tmp/linkbox/storage/" ; With trailing /
                       :id-file "/tmp/linkbox/id"
                       :targets-file "/tmp/linkbox/targets"
                       :port 5000
                       :auth "test")) ; Will be used for authentication

;; It should now be running. Example use:

; $ curl http://localhost:5000/ --form auth=test --form url=http://joram.io
; $ curl http://localhost:5000/ --form auth=test --form file=@file.extension

;; On success you'll receive a URL that either redirects to the target or
;; displays the file.
```

You could set up something like Nginx to try serving files from the directory
passed into `:path` before directing them to the Linkbox server.

## License

    Copyright (c) 2015 Joram Schrijver

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
