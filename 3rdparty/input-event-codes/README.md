# Input Event Codes
[![Quicklisp status](https://quickdocs.org/badge/input-event-codes.svg)](https://quickdocs.org/input-event-codes)

This little library is a port of all constants found in `input-event-codes.h`,
an event code header file found on both
[Linux](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/include/uapi/linux/input-event-codes.h?h=v5.19-rc8)
and
[FreeBSD](https://cgit.freebsd.org/src/tree/sys/dev/evdev/input-event-codes.h).

```lisp
(require :input-event-codes)

;; Print all constants and their values
(do-external-symbols (sym :input-event-codes)
  (format t "~S = ~D" sym (symbol-value sym)))

;; Convoluted example...
(defun handle-button-event (code pressed-p)
  (when (= btn input-event-codes:+btn-left+)
    ...))
```

There is currently only support for Linux and FreeBSD. The library uses
trivial-features to conditionally load the appropriate constant set. If this or
a similar header is found in other operating systems, please let me know and I
will add them. :)

## Native Documentation

The Linux Kernel docs [provides an outline](https://docs.kernel.org/input/event-codes.html) over all the event code constants.

## OS Differences

- **+sw-max+** and **+sw-cnt+** is `#xf` and `#x10` respectively on FreeBSD,
  but `#x10` and `#x11` respectively on Linux.

These constants are available on Linux only:

- **+key-sidevu-sonar+**
- **+key-all-applications+**
- **+key-autopilot-engage-toggle+**
- **+key-clearvu-sonar+**
- **+key-dual-range-radar+**
- **+key-fishing-chart+**
- **+key-previous-element+**
- **+key-notification-center+**
- **+key-brightness-menu+**
- **+key-nav-chart+**
- **+key-radar-overlay+**
- **+key-dictate+**
- **+key-mark-waypoint+**
- **+key-nav-info+**
- **+key-single-range-radar+**
- **+sw-machine-cover+**
- **+key-sos+**
- **+key-traditional-sonar+**
- **+key-next-element+**
- **+key-hangup-phone+**
- **+key-fn-right-shift+**
- **+key-pickup-phone+**
- **+key-emoji-picker+**
