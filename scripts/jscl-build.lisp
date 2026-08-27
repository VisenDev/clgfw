(jscl:compile-application (list "src/package.lisp"
                                "src/color.lisp"
                                "src/color-constants.lisp"
                                "src/common.lisp"
                                "src/backend-web.lisp"
                                "example/hello.lisp"
                                "scripts/jscl-invoke-main.lisp")
                          "clgfw.js")

