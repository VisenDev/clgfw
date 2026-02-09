# POSIX shared memory FFI Reference

While **posix-shm** aims to be closer to a Lisp library with its safety and quality-of-life features,
it provides an FFI *stratum* accessible via the package and system both named `posix-shm/ffi` for those who prefer a slimmer API closer to its native environment.

This reference lists all types, symbols, and functions exposed by the FFI and aims to emphasize differences between this and the C API.
I strongly recommend the Linux [manpages](https://linux.die.net/man/7/shm_overview) for details about the functions themselves.

## Types

### [Foreign Type] **blkcnt**

`blkcnt_t` in C. Used for file block counts

### [Foreign Type] **blksize**

`blksize_t` in C. Used for file block sizes

### [Foreign Type] **dev**

`dev_t` in C. Used for device IDs

### [Foreign Type] **gid**

`gid_t` in C. Used to hold group IDs

### [Foreign Type] **ino**

`ino_t` in C. Used to hold inode numbers

### [Foreign Type] **nlink**

`nlink_t` in C. Used to hold the number of links to a file

### [Foreign Type] **off**

`off_t` in C. Used for file sizes

### [Foreign Type] **size**

`size_t` in C. Used to hold a count of bytes

### [Foreign Type] **uid**

`uid_t` in C. Used to hold user IDs

### [Constant Enum] **c-error**

Enumeration of all possible `*errno*` values set by syscalls from the POSIX API.
All **c-error** values are keywords of the same name as their C counterparts,
e.g. `EEXIST` in C is `:eexist` in the FFI.

### [Foreign Bitfield] **open-flags**

Options for opening a POSIX shm object.

A valid *open-flags* list is created by appending exactly one of `:rdonly` or `:rdwr` to any of the other flags listed here:

- `:rdonly` - Open the object for read access. A shared object opened this way can be **mmap**ed only for read (`:read`) access.
- `:rdwr` - Open the object for read-write access.
- `:creat` - Create the memory object if it does not exist.
- `:excl` - If `:creat` is also specified, return an error if the file already exists.
- `:trunc` - If the shared memory object already exists, truncate it to zero bytes.

### [Foreign Bitfield] **mode**

Access permissions for the shared memory object:

- `:user-all`, `:rwxu` - The user owner is permitted read, write, and exec access.
- `:user-read`, `:rusr` - The user owner is permitted read access.
- `:user-write`, `:wusr` - The user owner is permitted write access.
- `:user-exec`, `:xusr` - The user owner is permitted exec access.
- `:group-all`, `:rwxg` - The group owner is permitted read, write, and exec access.
- `:group-read`, `:rgrp` - The group owner is permitted read access.
- `:group-write`, `:wgrp` - The group owner is permitted write access.
- `:group-exec`, `:xgrp` - The group owner is permitted exec access.
- `:other-all`, `:rwxo` - Others are permitted read, write, and exec access.
- `:other-read`, `:roth` - Others are permitted read access.
- `:other-write`, `:woth` - Others are permitted write access.
- `:other-exec`, `:xoth` - Others are permitted exec access.

### [Foreign Bitfield] **prot-flags**

The desired memory protection of an **mmap**ed mapping.

A valid *prot-flags* list is either `(:prot-none)` or a list of one or more of the following flags:

- `:exec` - Pages may be executed
- `:read` - Pages may be read
- `:write` - Pages may be written
- `:none` - Pages may not be accessed

### [Foreign Bitfield] **mmap-flags**

For use in **mmap**. See `mmap(2)`

- `:shared`
- `:private`
- `:anonymous`
- `:fixed`
- `:growsdown`
- `:noreserve`

### [Foreign Structure] **stat**

The call to **fstat** writes to a *stat* structure, which contains the following slots:

- *st-dev* - ID of device containing file
- *st-ino* - Inode number
- *st-mode* - File type and mode
- *st-nlink* - Number of hard links
- *st-uid* - User ID of owner
- *st-gid* - Group ID of owner
- *st-rdev* - Device ID (if special file)
- *st-size* - Total size, in bytes
- *st-blksize* - Block size for filesystem I/O
- *st-blocks* - Number of 512B blocks allocated
- *st-mtime* - Modified time
- *st-atime* - Access time
- *st-ctime* - Creation time

## Functions and Variables

### [Accessor Symbol Macro] **`*errno*`** => *c-error*

`errno` in C. When the return value of a call indicates an error (usually `-1`, `(cffi:null-pointer)`, or `+map-failed+` in the FFI),
**`*errno*`** is generally modified to indicate the cause of error.

**`*errno*`** is thread-local; setting it in one thread does not affect its value in any other thread.

### [Constant Variable] **+map-shared+**

The sole POSIX flag for use with shared memory objects.

### [Constant Variable] **+map-failed+**

On failure, the function **mmap** returns **+map-failed+**, and **`*errno*`** is set to indicate the error.

### [Function] **strerror** *c-error* => *string*

Returns a string describing the error code.

### [Function] **shm-open** *name oflag mode* => *shm*

See `shm_open(3)`

### [Function] **ftruncate** *fd length* => *integer*

See `ftruncate(2)`

### [Function] **mmap** *addr length prot flags fd offset* => *pointer*

See `mmap(2)`

### [Function] **munmap** *addr length* => *integer*

See `munmap(2)`

### [Function] **shm-unlink** *name* => *integer*

See `shm_unlink(2)`

### [Function] **close** *fd* => *integer*

See `close(2)`

### [Function] **fstat** *fd statbuf* => *integer*

See `fstat(2)`

### [Function] **fchown** *fd owner group* => *integer*

See `fchown(2)`

### [Function] **fchmod** *fd mode* => *integer*

See `fchmod(2)`
