# POSIX shared memory for Common Lisp
[![builds.sr.ht status](https://builds.sr.ht/~shunter/posix-shm/commits/test.yml.svg)](https://builds.sr.ht/~shunter/posix-shm/commits/test.yml)
[![Quicklisp status](https://quickdocs.org/badge/posix-shm.svg)](https://quickdocs.org/posix-shm)

Common Lisp bindings and wrapper for the POSIX shared memory API.

The POSIX shared memory (or `shm`) API "allows processes to communicate information by sharing a region of memory." (`shm_overview(7)`).
This library provides two *strata* to access the POSIX shm API:

- The package `posix-shm/ffi`, a collection of slim bindings to the POSIX API. [FFI Reference](./doc/ffi-reference.md)
- The package `posix-shm`, a lispy wrapper around the FFI that integrates more closely to the features of Common Lisp, and provides a handful of utilities and macros. [API Reference](./doc/api-reference.md)

Features include:

- Open, close, create, resize, change ownership of, change permissions of, and memory map to shared memory objects.
- **open-shm** appears more like **open** from the standard library.
- **open-shm\***, for creating anonymous shm objects.
- **with-open-shm**, **with-mmap** and similar **with-** macros for safely accessing resources with dynamic extent.

## Installation and Usage

Install posix-shm with Quicklisp:

```lisp
* (ql:quickload :posix-shm)
* (use-package :posix-shm)

* (defparameter +shm-size+
    (* 10 (cffi:foreign-type-size :int)))
+SHM-SIZE+

* (defvar *shm*)
*SHM*
* (setf *shm* (open-shm "/foobar-shm" :direction :io :if-does-not-exist :create
                        :permissions '(:user-read :user-write)))
#<SHM for "/foobar-shm" {10056A8F03}>
* (print (shm-fd *shm*))
4 ;; Runtime-dependent

* (truncate-shm *shm* +shm-size+)

* (defparameter *ptr1*
    (mmap-shm *shm* +shm-size+ :prot '(:write)))
*PTR1*

* (defparameter *ptr2*
    (mmap-shm *shm* +shm-size+ :prot '(:read)))
*PTR2*

* (dotimes (i 10)
    (setf (cffi:mem-aref *ptr1* :int i) (* i 10)))
* (dotimes (i 10)
    (print (cffi:mem-aref *ptr2* :int i)))
0
10
20
30
40
50
60
70
80
90
NIL

* (munmap *ptr1* +shm-size+)
* (munmap *ptr2* +shm-size+)
* (close-shm *shm*)
T
* (delete-shm "/foobar-shm")
```

Use **with-open-shm** and **with-mmap** to automatically close and munmap when the program leaves scope:

```lisp
(with-open-shm (shm "/foobar-shm" :direction :io)
  (truncate-shm shm 100)
  (with-mmap (ptr shm 100)
    ;; do your thing...
    ))
```

**with-open-shm-and-mmap** opens a shm, truncates it, and then maps it to a pointer all in one:

```lisp
(with-mmapped-shm (shm ptr ("/foobar-shm" :direction :io) (100))
  ;; do your thing...
  )
```

Use **open-shm\*** and **with-open-shm\*** to create anonymous shm objects that are unlinked the moment they are created:

```lisp
(defvar *anon-shm* (open-shm* :direction :io :permissions '(:user-all))
;; do your thing...
(close-shm *anon-shm*)

(with-open-shm* (anon-shm :direction :io :permissions '(:user-all))
  ;; do your thing...
  )
```

## Contributing

Any comments, questions, issues, or patches are greatly appreciated!
I do my main development on [Sourcehut](https://sr.ht/~shunter/posix-shm/), with a [mailing list](https://lists.sr.ht/~shunter/public-inbox) and [issue tracker](https://todo.sr.ht/~shunter/posix-shm).

## Reference

Descriptions of types, classes, functions, and macros of the high-level API are in the [API Reference](./doc/api-reference.md).

Descriptions of the lower-level foreign types and bindings are in the [FFI Reference](./doc/ffi-reference.md).
