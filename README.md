# clgfw

WIP

This project is an experimental implementation of a cross platform windowing library
in pure common lisp, similar to how GLFW is a cross platform windowing in C.

`clgfw` is different from other common lisp windowing libraries in that it is not just
a wrapper over an existing C windowing/ui library like GLFW, SDL2, QT, etc... Rather, 
`clgfw` interfaces with platform specific windowing functionality directly. This means
you can ship self contained common lisp executables, without needing to bundle any shared
libraries along with your binary. The only libraries used in clgfw should be standard
system libraries for your platform.

# Provided Backends as of time of writing (this may become outdated in the future)

- X11: Implemented using [clx](https://github.com/sharplispers/clx)
- Wayland: Implemented using [wayflan](https://sr.ht/~shunter/wayflan)
- JVM: The java AWT, requires Armed Bear Common Lisp
- Curses: Terminal user interface, requires ncurses to be available on your platform, written using [cl-charms](https://github.com/HiTECNOLOGYs/cl-charms)

# Planned or Theorized Backends
- Win32: Hopefully the ftw library can be used for this, I don't have a windows computer so I haven't worked on this yet. 
- Cocoa/clozure: Using the Clozure ObjC Bridge
- Cocoa/ecl: Using [silicon.h](https://github.com/EimaKve/Silicon) statically embedded in an ECL image
- CLOG: WebUI backend using [CLOG](https://github.com/rabbibotton/clog)
- JSCL: WebUI backend using JSCL to compile common lisp to JavaScript
- Kitty: TUI backend using the Kitty Graphics Protocol and Kitty Keyboard Protocol for increased functionality
- CAPI: Backend using Lispworks built-in windowing library, would require Lispworks

# Usage
Look at the `src/package.lisp` file for an updated index of exported symbols. Also look
at examples

# License
Copyright 2026 Robert Wess Burnett

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
