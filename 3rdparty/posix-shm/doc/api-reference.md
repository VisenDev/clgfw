# POSIX shared memory API Reference

## Types and Classes

### [Standard Class] **shm**

**Description:**

A POSIX shared memory object. Instances of __shm__ respond to the reader **shm-name**.

### [Standard Class] **open-shm**

**Description:**

An open shared memory object. Instances of __open-shm__ respond to the readers **shm-fd** and **shm-name**.

### [Standard Class] **closed-shm**

**Description:**

When an instance of __shm__ is closed, its class is changed to __closed-shm__ and its file descriptor slot is removed. Instances of __closed-shm__ respond to the reader function __shm-name__.

### [Structure Class] **stat**

The return value of **stat-shm** is a **stat** structure record, which responds to the following readers:

- **stat-dev** - the ID of the device containing the file
- **stat-ino** - inode number
- **stat-mode** - File type and mode
- **stat-nlink** - Number of hard links
- **stat-uid** - User ID of owner
- **stat-gid** - Group ID of owner
- **stat-rdev** - Device ID (if special file)
- **stat-size** - Total size, in bytes
- **stat-blksize** - BLock size for filesystem I/O
- **stat-blocks** - Number of 512B blocks allocated

### [Condition Class] **shm-error**

**Class Precedence List:**

__shm-error__, __error__, __serious-condition__, __condition__, __t__

**Description:**

A __shm-error__ occurs during opening or closing a shared memory object, or some low-level transaction between the OS and a preexisting shared memory object. Instances of __shm-error__ responds to the reader function **shm-error-name**.

### [Condition Class] **shm-does-not-exist**

**Class Precedence List:**

__shm-does-not-exist__, __shm-error__, __error__, __serious-condition__, __condition__, __t__

**Description:**

A __shm-does-not-exist__ error condition is signaled within a call to **open-shm** or **delete-shm**, when a shared memory object identified by *name* does not exist, and *if-does-not-exist* is `:error`.

### [Condition Class] **shm-exists**

**Class Precedence List:**

__shm-exists__, __shm-error__, __error__, __serious-condition__, __condition__, __t__

**Description:**

A __shm-exists__ error condition is signaled within a call to **open-shm** or **open-shm\***,
when a shared memory object identified by *name* already exists, and *if-exists* is `:error`.

### [Condition Class] **mmap-error**

**Class Precedence List:**

__mmap-error__, __error__, __serious-condition__, __condition__, __t__

**Description:**

An __mmap-error__ error condition is signaled within a call to **mmap** or **munmap** whenever an operation fails.

## Functions and Macros

### [Function] **open-shm** *name &key direction if-exists if-does-not-exist permissions* => *shm*

**Arguments and Values:**

- *name* -- a __string__.
- *direction* -- one of `:input` or `:io`. The default is `:input`.
- *if-exists* -- one of `:error`, `:overwrite`, `:truncate`, `:supersede`, or `nil`. The default is `:overwrite`.
- *if-does-not-exist* -- one of `:error`, `:create`, or `nil`. The default is `:error` if *direction* is `:input` or *if-exists* is one of `:overwrite` or `:truncate`; the default is `:create` otherwise.
- *permissions* -- a list of __mode__ values. See __mode__ from the [FFI reference](./ffi-reference.md).
- *shm* -- a __shm__.

**Description:**

Creates and opens a new, or opens an existing, shared memory object.
It can then be used by separate processes to **mmap** the same region of shared memory.

If *direction* is `:input`, it opens the shm object for read access (in C, this would be the `O_RDONLY` flag).
If it is `:io`, it it opened for read-write access (`O_RDWR`).

*if-exists* specifies the action to be taken if a shm object of the name *name* already exists.
If *direction* is `:input`, it is ignored:

- `:error` - An error of type **shm-exists** is signaled.
  If *if-does-not-exist* is not `:create`, then this is ignored.
- `:overwrite` - Output operations on the shm object destructively modify the shared memory.
- `:truncate` - Like `:overwrite`, but the shm object is first truncated to 0 bytes first (in C, this would be `O_TRUNC`).
- `:supersede` - The existing shm object is superseded; that is, the old shm object is unlinked, and then a new shm object is created.
  If *if-does-not-exist* is not `:create`, then this is ignored.
- `nil` - No shm object is created; instead, `nil` is returned to indicate failure.
  If *if-does-not-exist* is not `:create`, then this is ignored.

*if-does-not-exist* specifies the action to be taken if a shm object of the name *name* does not exist.

- `:error` - An error of type **shm-does-not-exist** is signaled.
- `:create` - An empty shm object is created (in C, this would be `O_CREAT`).
- `nil` - No shm object is created; instead, `nil` is returned to indicate failure.

Translating from C, each combination of `oflag` in the C call would map to:

- `O_RDONLY` - `` (no keyword arguments)
- `O_RDONLY | O_CREAT` - `:if-does-not-exist :create`
- `O_RDONLY | O_CREAT | O_EXCL` - `:if-exists :error :if-does-not-exist :create`
- `O_RDONLY | O_TRUNC` - `:if-exists :truncate`
- `O_RDONLY | O_CREAT | O_TRUNC` - `:if-exists :truncate :if-does-not-exist :create`
- `O_RDONLY | O_CREAT | O_EXCL | O_TRUNC` - `:if-exists :error :if-does-not-exist :create`
- `O_RDWR` - `:direction :io`
- `O_RDWR | O_CREAT` - `:direction :io :if-does-not-exist :create`
- `O_RDWR | O_CREAT | O_EXCL` - `:direction :io :if-exists :error`
- `O_RDWR | O_TRUNC` - `:direction :io :if-exists :truncate`
- `O_RDWR | O_CREAT | O_TRUNC` - `:direction :io :if-exists :truncate :if-does-not-exist :create`
- `O_RDWR | O_CREAT | O_EXCL | O_TRUNC` - `:direction :io :if-exists :error`

**Examples:**

```lisp
(open-shm name :direction :io) ;; => #<Read-write shm...>
(open-shm name :if-does-not-exist :create) ;; => #<Read-only shm...>
(setf s (open-shm name :direction :input)) ;; => #<Read-only shm...>
(shm-name s) ;; => name
```

**Exceptional Situations:**

**truncate-shm** signals an error of type __shm-error__ if the operation fails:

- If the operation fails because the file previously existsed, and *if-exists* is `:error`, then the error will be of type __shm-exists__.
- If the operation fails because the file previously did not exist, and *if-does-not-exist* is `:error`, then the error will be of type __shm-does-not-exist__.

### [Function] **open-shm\*** *name &key direction if-exists if-does-not-exist permissions* => *shm*

**Arguments and Values:**

- *direction* -- one of `:input` or `:io`. The default is `:input`.
- *permissions* -- a list of __mode__ values. See __mode__ from the [FFI Reference](./ffi-reference.md).
- *attempts* -- an unsigned __fixnum__.
- *shm* -- a __shm__.

**Description:**

Creates and opens, and then unlinks, a new shared memory object. This shm
object may be shared with another UNIX process by sending its file descriptor
over a UNIX domain socket via a `SCM_RIGHTS` control message (see `cmsg(3)`).

*attempts* defines the number of times the function attempts to open a shared memory object from a fresh generated name.

**Exceptional Situations:**

**open-shm\*** signals an error of type __shm-error__ if the operation fails. If the operation fails because **open-shm\*** ran out of attempts, the error will be of type __shm-exists__.

### [Function] **make-shm** *fd &optional name* => *shm*

**Arguments and Values:**

- *fd* -- a __fixnum__.
- *name* -- a __string__.
- *shm* -- a __shm__.

**Description:**

**make-shm** accepts a preexisting file descriptor to create a __shm__
instance.

### [Function] **shm-p** *object* => *boolean*

**Arguments and Values:**

- *object* -- an __object__.
- *boolean* -- a __generalized boolean__.

**Description:**

Returns __true__ when *object* is an instance of __shm__.

**Examples:**

```lisp
(defvar *shm* (open-shm*))
(shm-p *shm*) ;; => true
(shm-p 123) ;; => false
(close-shm *shm*)
(shm-p *shm*) ;; => true
```

### [Function] **open-shm-p** *object* => *boolean*

**Arguments and Values:**

- *object* -- an __object__.
- *boolean* -- a __generalized boolean__.

**Description:**

Returns __true__ when *object* is an instance of __open-shm__.

**Examples:**

```lisp
(defvar *shm* (open-shm*))
(shm-p *shm*) ;; => true
(shm-p 123) ;; => false
(close-shm *shm*)
(shm-p *shm*) ;; => false
```

### [Function] **truncate-shm** *shm size* => *no value*

**Arguments and Values:**

- *shm* -- a __shm__.
- *size* -- an unsigned __fixnum__.

**Description:**

Alter the size of the shared memory object to *size* bytes.
If *shm* was larger than *size*, the object shrinks. If *shm* was smaller than *size*, the object grows.

**Exceptional Situations:**

**truncate-shm** signals an error of type __shm-error__.

**Example:**

```lisp
(defun resize-shm-image-buffer (shm line-stride height)
  "Resize a SHM to fit a single image with the given stride and height."
  (shm-truncate shm (* line-stride heihgt)))
```

### [Function] **shm-stats** *shm* => *stats*

**Arguments and Values:**

- *shm* -- a __shm__.
- *stats* -- a __stat__.

**Description:**

**shm-stats** returns information about the shared memory object, in a __stat__
structure.

**Exceptional Situations:**

**shm-stats** signals an error of type __shm-error__.

### [Function] **chmod-shm** *shm permissions* => *no value*

**Arguments and Values:**

- *shm* -- a __shm__.
- *permissions* -- a list of __mode__ keywords. See __mode__ in the [FFI Reference](./ffi-reference.md).

**Description:**

**chmod-shm** changes the shared memory object's mode bits. The shm's mode describes whether the object's owner user, owner group, or any other can each access the shm for reading, writing, or execution.

**Exceptional Situations:**

**chmod-shm** signals an error of type __shm-error__.

### [Function] **close-shm** *shm* => **t**

**Arguments and Values:**

- *shm* -- a __shm__.

**Description:**

**close-shm** closes the shared memory object's underlying file descriptor, such that it no longer refers to any file and may be reused. A successful call to **close-shm** invalidates the value of **shm-fd** applied to *shm*, and changes its class to **closed-shm**.

**Exceptional Situations:**

**close-shm** signals an error of type __shm-error__.

### [Function] **delete-shm** *name* => **t**

**Arguments and Values:**

- *name* -- a __string__.

**Description:**

**delete-shm** removes a shared memory object name. Once all processes have unmapped the object from themselves, it is destroyed and resources recycle.

**Exceptional Situations:**

**delete-shm** signals an error of type __shm-error__ if the operation fails. If it fails because a shared memory object of that name does not exist, the error is of type __shm-does-not-exist__.

### [Function] **mmap-shm** *shm length &key addr protections flags offset* => *ptr*

**Arguments and Values:**

- *shm* -- a __shm__.
- *length* -- an unsigned __fixnum__.
- *addr* -- a CFFI __foreign-pointer__. By default, `(cffi:null-pointer)`
- *protections* -- a list of __prot-flags__ keywords. See __prot-flags__ from the [FFI Reference](./ffi-reference.md). By default, `'(:read :write)`
- *flags* -- a list of __mmap-flags__ keywords. See __mmap-flags__ from the [FFI Reference](./ffi-reference.md). By default, `(:shared)`.
- *offset* -- an unsigned __fixnum__. By default, 0
- *ptr* --  a CFFI __foreign-pointer__.

**Description:**

Creates a new mapping in the virtual address space of the process. The byte at `(cffi:mem-ref ptr :char)` maps to the first plus *offset* byte of the shared memory object, and so on.

*protections* describes the desired memory protection of the mapping.
It is a list of either `:none` or one or more of the following keywords:

- `:exec` - pages may be executed.
- `:read` - pages may be read.
- `:write` - pages may be written.

**Exceptional Situations:**

**mmap** signals an error of type __mmap-error__.

### [Function] **munmap** *addr size* => *no values*

**Arguments and Values:**

- *addr* -- a CFFI __foreign-pointer__.
- *size* -- an unsigned __fixnum__.

**Description:**

**munmap** deletes the mappings for the specified address range.

**Exceptional Situations:**

**munmap** signals an error of type __mmap-error__.

### [Macro] **with-open-shm** *(var name &rest options) &body body* => *result*

**Arguments and Values:**

- *var* -- a __variable name__.
- *name* -- a __string__.
- *options* -- __forms__; evaluated.
- *body* -- an __implicit progn__.
- *result* -- the __values__ returned by the *body*.

**Description:**

**with-open-shm** uses **open-shm** to create a connection to a shared memory object identified by *name*. *var* is bound to the new shared memory object during execution of the *body*.

When control leaves the body, either normally or abnormally, the shared memory object is automatically closed.

**Exceptional Situations:**

See the function **open-shm**.

### [Macro] **with-open-shm\*** *(var &rest options) &body body* => *result*

**Arguments and Values:**

- *var* -- a __variable name__.
- *options* -- __forms__; evaluated.
- *body* -- an __implicit progn__.
- *result* -- the __values__ returned by the *body*.

**Description:**

**with-open-shm\*** uses **open-shm\*** to create a connection to an anonymous shared memory object. *var* is bound to the new shared memory object during execution of the *body*.

When control leaves the body, either normally or abnormally, the shared memory object is automatically closed.

**Exceptional Situations:**

See the function **open-shm\***.

### [Macro] **with-mmap** *(var &rest options) &body body* => *result*

**Arguments and Values:**

- *var* -- a __variable name__.
- *options* -- __forms__; evaluated.
- *body* -- an __implicit progn__.
- *result* -- the __values__ returned by the *body*.

**Description:**

**with-mmap** uses **mmap** to create a virtual memory mapping to a shared memory object.. *var* is bound to the starting location of the memory map during execution of the *body*.

When control leaves the body, either normally or abnormally, the virtual memory mapping is automatically unmapped.

**Exceptional Situations:**

See the function **mmap**.

### [Macro] **with-open-shm-and-mmap** *(shm-var mmap-var (&rest shm-options) (&rest mmap-options) :key truncate) &body body* => *result*

**Arguments and Values:**

- *shm-var*, *mmap-var* -- __variable names__.
- *shm-options*, *mmap-options* -- __forms__; evaluated.
- *body* -- an __implicit progn__.
- *result* -- the __values__ returned by the *body*.

Uses **shm-open** to open a shm object, truncates it to `(+ LENGTH OFFSET)` (unless otherwise configured), and maps it with **mmap**.

When control leaves the body, either normally or abnormally, the shared memory object is automatically closed and the virtual memory mapping is automatically unmapped.

**Exceptional Situations:**

See the functions **open-shm** and **mmap**.

### [Macro] **with-open-shm-and-mmap\*** *(shm-var mmap-var (&rest shm-options) (&rest mmap-options) :key truncate) &body body* => *result*

**Arguments and Values:**

- *shm-var*, *mmap-var* -- __variable names__.
- *shm-options*, *mmap-options* -- __forms__; evaluated.
- *body* -- an __implicit progn__.
- *result* -- the __values__ returned by the *body*.

Uses **shm-open\*** to create an open an anonymous shm object, truncates it to `(+ LENGTH OFFSET)` (unless otherwise configured), and maps it with **mmap**.

When control leaves the body, either normally or abnormally, the shared memory object is automatically closed and the virtual memory mapping is automatically unmapped.

**Exceptional Situations:**

See the functions **open-shm\*** and **mmap**.
