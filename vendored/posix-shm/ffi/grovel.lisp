;;; ffi/grovel.lisp -- POSIX shared memory grovel definitions
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(include "sys/mman.h" "sys/stat.h" "sys/time.h" "fcntl.h" "unistd.h" "string.h" "errno.h")

(in-package #:xyz.shunter.posix-shm.ffi)

(ctype blkcnt "blkcnt_t")
(ctype blksize "blksize_t")
(ctype dev "dev_t")
(ctype gid "gid_t")
(ctype ino "ino_t")
(ctype nlink "nlink_t")
(ctype off "off_t")
(ctype size "size_t")
(ctype ssize "ssize_t")
(ctype uid "uid_t")
(ctype time "time_t")

(bitfield mode
  ((:user-all "S_IRWXU"))
  ((:user-read "S_IRUSR"))
  ((:user-write "S_IWUSR"))
  ((:user-exec "S_IXUSR"))
  ((:group-all "S_IRWXG"))
  ((:group-read "S_IRGRP"))
  ((:group-write "S_IWGRP"))
  ((:group-exec "S_IXGRP"))
  ((:other-all "S_IRWXO"))
  ((:other-read "S_IROTH"))
  ((:other-write "S_IWOTH"))
  ((:other-exec "S_IXOTH"))

  ((:rwxu "S_IRWXU"))
  ((:rusr "S_IRUSR"))
  ((:wusr "S_IWUSR"))
  ((:xusr "S_IXUSR"))
  ((:rwxg "S_IRWXG"))
  ((:rgrp "S_IRGRP"))
  ((:wgrp "S_IWGRP"))
  ((:xgrp "S_IXGRP"))
  ((:rwxo "S_IRWXO"))
  ((:roth "S_IROTH"))
  ((:woth "S_IWOTH"))
  ((:xoth "S_IXOTH")))

(cstruct timespec "struct timespec"
  (tv-sec "tv_sec" :type time)
  (tv-nsec "tv_nsec" :type :long))

(cstruct stat "struct stat"
  (st-dev "st_dev" :type dev)
  (st-ino "st_ino" :type ino)
  (st-mode "st_mode" :type mode)
  (st-nlink "st_nlink" :type nlink)
  (st-uid "st_uid" :type uid)
  (st-gid "st_gid" :type gid)
  (st-rdev "st_rdev" :type dev)
  (st-size "st_size" :type off)
  (st-blksize "st_blksize" :type blksize)
  (st-blocks "st_blocks" :type blkcnt)

  #-darwin
  (st-atim "st_atim" :type (:struct timespec))
  #+darwin
  (st-atim "st_atimespec" :type (:struct timespec))
  #-darwin
  (st-mtim "st_mtim" :type (:struct timespec))
  #+darwin
  (st-atim "st_mtimespec" :type (:struct timespec))
  #-darwin
  (st-ctim "st_ctim" :type (:struct timespec))
  #+darwin
  (st-atim "st_ctimespec" :type (:struct timespec)))

(constantenum c-error
  ((:eacces "EACCES")
   :documentation "Permission denied")
  ((:eagain "EAGAIN")
   :documentation "Resource temporarily unavailable")
  ((:ebadf "EBADF")
   :documentation "Not a valid file desciptor")
  ((:eexist "EEXIST")
   :documentation "File exists")
  ((:efbig "EFBIG")
   :documentation "File too large")
  ((:eintr "EINTR")
   :documentation "Interrupted function call")
  ((:einval "EINVAL")
   :documentation "Invalid argument")
  ((:eio "EIO")
   :documentation "Input/output error")
  ((:emfile "EMFILE")
   :documentation "The per-process open fd limit has been reached")
  ((:enametoolong "ENAMETOOLONG")
   :documentation "Name too long")
  ((:enfile "ENFILE")
   :documentation "The system-wide open fd limit has been reached")
  ((:enoent "ENOENT")
   :documentation "No such file or directory")
  ((:enomem "ENOMEM")
   :documentation "Cannot allocate enough memory")
  ((:eoverflow "EOVERFLOW")
   :documentation "Value too large to be stored in data type")
  ((:eperm "EPERM")
   :documentation "Operation not permitted"))

(bitfield open-flags
  ((:rdonly "O_RDONLY")
   :documentation "Open the object for read access")
  ((:rdwr "O_RDWR")
   :documentation "Open the object for read-write access")
  ((:creat "O_CREAT")
   :documentation "Create the shared memory object if it does not exist")
  ((:excl "O_EXCL")
   :documentation "If :OCREAT is specified, and a shared memory object with the given name already exists, return an error.")
  ((:trunc "O_TRUNC")
   :documentation "If the shared memory already exists, truncate it to zero bytes"))

(bitfield prot-flags
  ((:exec "PROT_EXEC")
   :documentation "Pages may be executed")
  ((:read "PROT_READ")
   :documentation "Pages may be read")
  ((:write "PROT_WRITE")
   :documentation "Pages may be written")
  ((:none "PROT_NONE")
   :documentation "Pages may not be accessed"))

(bitfield mmap-flags
  ((:shared "MAP_SHARED"))
  ((:private "MAP_PRIVATE"))
  ((:anonymous "MAP_ANONYMOUS" "MAP_ANON"))
  ((:fixed "MAP_FIXED"))
  ((:growsdown "MAP_GROWSDOWN"))
  ((:noreserve "MAP_NORESERVE")))

(constantenum whence
  ((:set "SEEK_SET"))
  ((:cur "SEEK_CUR"))
  ((:end "SEEK_END")))

(cvar ("errno" *errno*) c-error)
(constant (+map-shared+ "MAP_SHARED"))
(constant (+map-failed+ "MAP_FAILED"))
