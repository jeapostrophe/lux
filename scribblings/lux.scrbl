#lang scribble/manual
@require[@for-label[lux
                    lux/chaos
                    lux/chaos/gui
                    lux/chaos/gui/val
                    lux/chaos/gui/key
                    lux/chaos/gui/mouse
                    racket/contract
                    pict/convert
                    racket/gui/base
                    racket/base]]

@title{lux: brilliant interactive programs}
@author{Jay McCarthy}

@defmodule[lux]

The @racketmodname[lux] module provides an efficient way to build
interactive programs that consist of plain mathematical functions. It
is comparable to @racketmodname[2htdp/universe], although designed to
allow more parameterization of how the program interacts.

Check out some examples in the
@link["https://github.com/jeapostrophe/lux/tree/master/examples"]{examples}
directory in the source.

@local-table-of-contents[]

@section{Structure of a @racketmodname[lux] Program}

A @racketmodname[lux] program chooses how it will interact be
selecting a @tech{chaos} and calling @racket[call-with-chaos] with
that @tech{chaos} and a thunk that calls @racket[fiat-lux] with a
@tech{word}. @racket[fiat-lux] may be called any number of nested
times within a call to @racket[call-with-chaos]. Each subsequent
@racket[fiat-lux] takes over the @tech{chaos} until the @tech{word}
completes. It is not typically possible to use @tech{word}s with
arbitrary @tech{chaos}es, as the @tech{chaos} specifies how the
@tech{word} interacts through events and output values.

When designing @tech{word}s, it is important to realize that
@tech{word} updating functions like @racket[word-tick] do not have to
return a @tech{word} of the same kind.

@defproc[(call-with-chaos [c chaos?] [t (-> any)]) any]{

Runs @racket[t] with @racket[c] as the current @tech{chaos}.}

@defproc[(fiat-lux [w word?]) any]{

Runs @racket[w] with the current @tech{chaos}.}

@section{The Word}

A @deftech{word} is a generic interface the encapsulates the
interactive behavior of a @racketmodname[lux] program.

@defproc[(word? [x any/c]) boolean?]{

Identifies @tech{word}s.}

@defthing[gen:word any/c]{

The generic interface binding for @tech{word}s.}

The @tech{word} methods are as follows:

@defproc[(word-fps [w word?]) flonum?]{

Returns the desired rate of updating for @racket[w] as
frames-per-second. By default, @racket[60.0] is returned.}

@defproc[(word-label [w word?] [frame-time flonum?]) string?]{

Returns a label for @racket[w] that could use @racket[frame-time] to
show the performance of the @tech{word} rendering. By default, returns
@racket[(lux-standard-label "Lux" frame-time)].}

@defproc[(word-evt [w word?]) evt?]{

Returns a synchronizable event that the @tech{word} @racket[w]
requires notification of. By default, returns @racket[never-evt].}

@defproc[(word-event [w word?] [e any/c]) word?]{

Returns a @tech{word} based on @racket[w] that integrates the
information from the event @racket[e]. The type of @racket[e] is
dependent on the current @tech{chaos}. If this returns @racket[#f],
then the @racketmodname[lux] programs stops. By default, returns
@racket[w].}

@defproc[(word-tick [w word?]) word?]{

Returns a @tech{word} based on @racket[w] after one tick of abstract
time. This will be called @racket[(word-fps w)] times per second. If
this returns @racket[#f], then the @racketmodname[lux] programs stops.
By default, returns @racket[w].}

@defproc[(word-output [w word?]) any/c]{

Returns the output value of @racket[w]. The type that this returns is
dependent on the current @tech{chaos}. By default, returns
@racket[#f]. @tech{chaos}es should always allow @racket[#f] and use it
to mean "no output".}

@defproc[(word-return [w word?]) any/c]{

Returns a value for @racket[w] when the @racketmodname[lux] programs
stops, which happens if @racket[word-event] or @racket[word-tick]
return @racket[#f].}

@subsection{Helpers}

@defproc[(lux-standard-label [s string?] [frame-time flonum?]) string?]{

Returns @racket[(string-append s ": " _fts)] where @racket[_fts]
formats @racket[frame-time] as milliseconds and as frames per
second.}

@section{Chaos}

A @deftech{chaos} is generic interface for an empty manifestation of
an interactive space that is given form by the @tech{word} and
@racket[fiat-lux].

@subsection{Racket GUI Chaos}

@defmodule[lux/chaos/gui]

This module provides the standard @tech{chaos} that most users of
@racketmodname[lux] will use.

@defproc[(make-gui [#:mode mode (or/c (one-of/c 'draw 'gl-compat 'gl-core)
                                      (is-a?/c gl-config%))
                    'draw]
                   [#:opengl-hires? opengl-hires? boolean? #f]
                   [#:start-fullscreen? start-fullscreen? boolean?
                    #f]
                   [#:frame-style frame-style (listof symbol?) '()]
                   [#:icon icon
                           (or/c #f path-string? (is-a?/c bitmap%))
                           #f]
                   [#:x x
                    (or/c exact-nonnegative-integer? (one-of/c 'left 'center 'right))
                    'center]
                   [#:y y
                    (or/c exact-nonnegative-integer? (one-of/c 'top 'center 'bottom))
                    'center]
                   [#:width width
                            exact-nonnegative-integer?
                            800]
                   [#:height height
                             exact-nonnegative-integer?
                             600]
                   [#:monitor monitor
                    (or/c false/c exact-nonnegative-integer?)
                    #f])
         chaos?]{

Returns a @tech{chaos} that opens a GUI frame
 with a canvas to draw
on.
The frame is placed at position @racket[x],@racket[y] on monitor @racket[monitor];
if @racket[monitor] is @racket[#f], the monitor containing the mouse pointer is used.
 The default size of the frame is
@racket[width]x@racket[height]. The icon for the application is set to
@racket[icon]. If @racket[start-fullscreen?] is true, then the frame
is initially fullscreen. The frame's style is set to
@racket[frame-style].

The canvas is set up for drawing based on @racket[mode]. If
@racket[mode] is @racket['draw], then the canvas assumes that
@racketmodname[racket/draw] is used. If other values are used, then
the canvas is drawn with OpenGL. If @racket[mode] is
@racket['gl-compat], then a compatibility OpenGL profile is used. If
@racket[mode] is @racket['gl-core], then a core OpenGL profile is
used. If @racket[mode] is a @racket[gl-config%] object, then it is
used to initialize the canvas. If @racket[opengl-hires?] is
@racket[#t], then the resulting @racket[gl-config%] object will have
high resolution mode set.

The values that @racket[word-event] is called with are either
@racket['close] (for when the window's close button is pressed), a
@racket[key-event%] object for when keys are pressed, or a
@racket[mouse-event%] object for when the mouse is used.

The values that @racket[word-output] should return are functions that
satisfy the contract @racket[(-> real? real? (is-a?/c dc<%>) any)]
where the first argument is the width of the canvas, the second is the
height, and the third is the canvas's drawing context.}

@subsubsection{Drawing Values}

@defmodule[lux/chaos/gui/val]

This module provides a helpful function for drawing functional images
with @racketmodname[lux/chaos/gui].

@defproc[(make-gui/val [#:scale? scale? boolean? #t])
         (-> pict-convertible?
             (-> real? real? (is-a?/c dc<%>) any))]{

Produces a function that draws @racket[pict-convertible?] values on to
@racketmodname[lux/chaos/gui]'s drawing context. If @racket[scale?] is
true, then the value will be scaled to file the drawing context (while
preserving aspect ratio), otherwise the value will be drawn in the
center as-is.}

@subsubsection{Tracking Keyboard State}

@defmodule[lux/chaos/gui/key]

This module provides a set of functions for tracking keyboard state
for use inside of @racket[word-tick], rather than updating word state
with each event as in @racket[word-event]. Such as system may be
appropriate for interactive programs where input is only has an impact
at a consistent tick rate.

@defstruct*[key-state ([keys hash?] [shift? boolean?] [control? boolean?]
                       [meta? boolean?] [alt? boolean?] [mod3? boolean?]
                       [mod4? boolean?] [mod5? boolean?])]{

Stores a mapping of which keys are presently pressed.}

@defproc[(make-key-state) key-state?]{

Produces a @racket[key-state?] object with appropriate defaults.}

@defproc[(key-event? [x any/c])  boolean?]{

Identifies key events.}

@defproc[(key-event-code [ke key-event?]) (or/c (cons/c 'release (or/c char? key-code-symbol?)) (or/c char? key-code-symbol?))]{

Returns the code of the key event.}

@defproc[(key-state-update! [ks key-state?] [ke key-event?]) any]{

Updates @racket[ks] with @racket[ke].}

@defproc[(key-state-set? [ks key-state?] [kc (or/c char? key-code-symbol?)]) boolean?]{

Returns true if @racket[kc] is pressed in @racket[kc].}

@defproc[(key-state-set?! [ks key-state?] [kc (or/c char? key-code-symbol?)]) boolean?]{

Returns true if @racket[kc] is pressed in @racket[kc] and sets its
pressed status to false..}

@subsubsection{Tracking Mouse State}

@defmodule[lux/chaos/gui/mouse]

This module provides a set of functions for tracking mouse state
for use inside of @racket[word-tick], rather than updating word state
with each event as in @racket[word-event]. Such as system may be
appropriate for interactive programs where input is only has an impact
at a consistent tick rate.

@defstruct*[mouse-state ([x real?]
                         [y real?]
                         [left? boolean?]
                         [right? boolean?]
                         [middle? boolean?]
                         [shift? boolean?]
                         [control? boolean?]
                         [meta? boolean?]
                         [alt? boolean?]
                         [mod3? boolean?]
                         [mod4? boolean?]
                         [mod5? boolean?])]{

Stores the active state of the mouse.}

@defproc[(make-mouse-state) mouse-state?]{

Produces a @racket[mouse-state?] object with appropriate defaults.}

@defproc[(mouse-event? [x any/c])  boolean?]{

Identifies mouse events.}

@defproc[(mouse-event-xy [me mouse-event?]) (values real? real?)]{

Returns the position of the mouse event.}

@defproc[(mouse-state-update! [ms mouse-state?] [me mouse-event?]) any]{

Updates @racket[ms] with @racket[me].}

@subsection{Pair Chaos}

@defmodule[lux/chaos/pair]

This module provides a @tech{chaos} that pairs two other @tech{chaos}
objects for @racketmodname[lux] programs with multiple interfaces.

@defproc[(make-pair [left chaos?] [right chaos?]) chaos?]{

Returns a @tech{chaos} where the input event type is the union of the
input events of @racket[left] and @racket[right] and the output type
is a pair of the output types of @racket[left] and @racket[right].}

@subsection{Implementing a Chaos}

@defmodule[lux/chaos]

Users of @racketmodname[lux] will probably not need to implement
@tech{chaos}es, but will use those that are standard.

@defproc[(chaos? [x any/c]) boolean?]{

Identifies @tech{chaos}s.}

@defthing[gen:chaos any/c]{

The generic interface binding for @tech{chaos}es.}

The @tech{chaos} methods are as follows:

@defproc[(chaos-start! [c chaos?]) any]{

Called at the start of using @racket[c] as the current
@tech{chaos}. By default, does nothing.}

@defproc[(chaos-yield [c chaos?] [e evt?]) any]{

Synchronizes on @racket[e] in a way safe for @racket[c]. By default,
calls @racket[sync].}

@defproc[(chaos-event [c chaos?]) evt?]{

Returns an event that when ready returns a @racket[c] event value. By
default, returns @racket[never-evt].}

@defproc[(chaos-output! [c chaos?] [o any/c]) any]{

Outputs @racket[o] to @racket[c]. @tech{chaos}es should always allow
@racket[#f] and use it to mean "no output". By default, does nothing.}

@defproc[(chaos-label! [c chaos?] [s string?]) any]{

Outputs @racket[s] as the label of @racket[c]. By default, does
nothing.}

@defproc[(chaos-swap! [c chaos?] [t (-> any)]) any]{

Calls @racket[t] while preparing @racket[c] to run a different
@tech{word}. By default, just calls @racket[t].}

@defproc[(chaos-stop! [c chaos?]) any]{

Called at the end of using @racket[c] as the current @tech{chaos}. By
default, does nothing.}
