unit module FileVarFixture;

# `$?FILE` is a compile-time constant of the compilation unit it appears in, so
# every routine here must report THIS file, however deep in the program the call
# comes from.
sub fixture-file() is export { $?FILE }

sub fixture-file-interpolated() is export { "[$?FILE]" }

sub fixture-file-nested() is export { inner-file() }
sub inner-file() { $?FILE }

# The file `callframe` reports for a frame is the file that frame's code was
# defined in, so `fixture-frame-file`'s own frame must report this module.
sub fixture-frame-file() is export { inner-frame-file() }
sub inner-frame-file() { callframe(1).file }

# A routine passed in from the caller's file keeps reporting the caller's file,
# even though it is invoked from here.
sub fixture-invokes(&cb) is export { cb() }

# The shape rakudo's Test.rakumod uses to attribute a failure to whoever called
# the assertion: walk out of my own file and report the first frame that is not
# in it.
sub fixture-caller-outside() is export { walk-out() }
sub walk-out() {
    my $level = 0;
    my $frame;
    repeat {
        $frame = callframe(++$level);
    } while $frame.defined && $frame.file eq $?FILE;
    $frame.defined ?? $frame.file !! 'walked off the stack';
}
