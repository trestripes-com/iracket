#lang scribble/manual
@(require racket/runtime-path
          scribble/manual
          scribble/basic
          (for-label racket/base racket/contract iracket/install))

@title{IRacket: Racket Kernel for Jupyter}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

This library provides a Racket kernel for @hyperlink["http://jupyter.org/"]{Jupyter},
enabling interactive notebook-style programming with Racket.


@section[#:tag "install"]{Installing the IRacket Jupyter Kernel}

After installing the @tt{iracket} package, you must register the IRacket kernel
with Jupyter. Kernel registration can be done either through the @exec{raco
iracket install} command or by using the @racketmodname[iracket/install] module.

If @exec{racket} and @exec{jupyter} are both in your executable search
path, then you can register the kernel by either of the following:
@itemlist[
@item{at the command line: @exec{raco iracket install}, or}
@item{in Racket: @racket[(begin (require iracket/install) (install-iracket!))]}
]

If the @exec{jupyter} command is not in your executable search path, you must
tell the installer the absolute path to the @exec{jupyter} executable using
the @exec{--jupyter-exe} flag or @racket[#:jupyter-exe] keyword argument. The
installer runs @exec{jupyter --data-dir} to find the directory where it
should install the IRacket kernel.

If the @exec{racket} command is not in your executable search path, or if you
want to register a kernel that uses a specific version of Racket, then you must
tell the installer the path to the executable to use. The executable must
support the same command-line interface that the @exec{racket} executable
supports, but it does not have to be named @litchar{racket}---for example, a
@exec{racketcgc} or @exec{racketcs} executable would also work. See the
output of @exec{raco iracket install --help} or the documentation for
@racket[install-iracket!] for details.

Note that if you register the kernel with a non-version-specific Racket command
(the default) and then change that command to run a different version of Racket
(for example, by changing your @tt{PATH} or by installing a new version of
Racket over the old one), then Jupyter will try to use the new version of Racket
to run the IRacket kernel. If the @tt{iracket} package is not installed in the
new version, this typically results in an error like ``collection not found for
module path: @racketmodname[(lib "iracket/iracket")]''. The same error will
occur if you try to run the kernel after removing the @tt{iracket} package.

@history[#:changed "1.1" @elem{Added @exec{raco iracket} command.}]

@subsection[#:tag "install-api"]{IRacket Installation API}

@defmodule[iracket/install]

@defproc[(install-iracket! [#:jupyter-exe jupyter-exe
                            (or/c (and/c path-string? complete-path?) #f) #f]
                           [#:racket-exe racket-exe
                            (or/c path-string? 'auto 'this-version) #f])
         void?]{

Registers the IRacket kernel in the location reported by @racket[jupyter-exe]
(with the @exec{--data-dir} flag). If @racket[racket-exe] is a path or
string, then it is included in the kernel registration as the command Jupyter
should use to run the kernel. If @racket[racket-exe] is @racket[#f], then the
registration uses the generic @exec{racket} command, but only if it is
included in the current executable search path; otherwise, an error is
raised. If @racket[racket-exe] is @racket['this-version], then the absolute path
to the currently running version of Racket is used.

@history[#:added "1.1"]
}
