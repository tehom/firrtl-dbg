This is an emacs interface for the FIRRTL debugger REPL.  To use
it, you will need Chisel3 and sbt.  If you don't know what those
are, you probably don't need this package.

The entry point is 'firrtl-dbg'.  You need to have already set up a
REPL in Scala.  Experiment on the usual guinea-pig project GCD, which
already has a REPL.  By default the correct command for GCD's REPL is
already at the top of the history list, but you'll have to point it
towards the right directory (the one that build.sbt lives in)

Instructions for setting up and using the FIRRTL REPL itself can be
found at
https://github.com/freechipsproject/chisel3/wiki/Debugging-with-the-Interpreter-REPL-1
https://github.com/freechipsproject/chisel3/wiki/Debugging-with-the-Interpreter-REPL-2
https://github.com/freechipsproject/chisel3/wiki/Debugging-with-the-Interpreter-REPL-3

This is just a more convenient interface to that.

You probably want to set firrtl-dbg-directory-history and
firrtl-dbg-repl-name-history to that your current project pops up
at the top of the history lists when calling firrtl-dbg.

To change an input value, left-click on the button next to it.  This
only works for input values.  To change how the value is displayed, eg
to make it a boolean or an enum, hit Alt-Enter on the button.

It doesn't support vpn scripts, but does support a native elisp
script.  Create it with firrtl-dbg-start-recording-script, do stuff
in the main buffer, then firrtl-dbg-stop-recording-script when
done.  It will pop up a buffer with the script in it.  It's on you
to copy that code somewhere.  Run it with firrtl-dbg-run-script.
You can also run handcrafted scripts, or scripts generated from
Scala but then you're going to have to write the print statements
and the conversion yourself.

At this point, there are a lot of features still to be added, and I
can't promise that I will ever get around to adding them.

Troubleshooting:

firrtl-dbg is not designed to catch circuit compilation errors.  On a
circuit that won't compile, firrtl-dbg will most likely just report
that it timed out.  It's a good practice to compile the circuit or run
its REPL at least once in sbt.

