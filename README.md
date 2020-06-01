This is an emacs interface for the Treadle debugger REPL.  To use it,
you will need Chisel3, Treadle, and sbt.  If you don't know what those
are, you probably don't need this package.

At this point, there are a lot of features still to be added, and I
can't promise that I will ever get around to adding them.

If you are looking for the older FIRRTL debugger REPL interface, it is
no longer developed.  The last version is in this repo at b10a8f8,
tagged last-firrtl-dbg

The entry point is 'treadle-dbg'.  You need to have already set up
Treadle in Scala.

Instructions for setting up and using Treadle itself can be found
at https://www.chisel-lang.org/treadle/

I suggest you experiment on the GCD, the usual guinea-pig project for
Chisel3.  If you have run any tests on GCD, and you have not changed
treadle-dbg-how-to-find-fir-file = always-use-latest-test, it will
mostly be ready.  As mentioned, you need to have already set up
Treadle in Scala.

You also need to point treadle-dbg at the right directory, the one
that build.sbt lives in.  You will probably want to set
treadle-dbg-directory-history so that your current project pops up at
the top of the history lists when calling treadle-dbg.

To change an input value, left-click on the button next to it.  This
only works for input values.

To change how the value is displayed, eg to make it a boolean or an
enum, hit Alt-Enter on the button.  That will bring up a customization
dialog, but it really stores the value in a file in the current
directory.

To make it step the circuit, either hit "S" or click the "Step"
button.

It doesn't support vpn scripts, but does support a native elisp
script.  Create it with firrtl-dbg-start-recording-script, do stuff in
the main buffer, then firrtl-dbg-stop-recording-script when done.  It
will pop up a buffer with the script in it.  It's on you to copy that
code somewhere.  Run it with firrtl-dbg-run-script.  You can also run
handcrafted scripts.  It's possible to generate scripts from Scala
with some hand-editing, I've done it, but you're going to have to
write the print statements and the conversion yourself.

Troubleshooting:

firrtl-dbg is not designed to catch circuit compilation errors.  On a
circuit that won't compile, firrtl-dbg will most likely just report
that it timed out.  It's a good practice to compile the circuit or run
its REPL at least once in sbt.

If wires of interest are not being displayed, it's probably because
they weren't doing anything and were optimized away.  This can happen
when you're debugging a trimmed-down version of your circuit to
test/debug/develop the logic.  What I found works is to add a printf
statement.  This guarantees that the wire is not optimized down to
nothing.  Like this:

if (debug) {
      when (fooWire) { printf("fooWire")}
    }
