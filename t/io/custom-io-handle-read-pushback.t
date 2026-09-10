use Test;

# `IO::Handle.read($n)` keeps whatever the handle's `READ` hands back BEYOND
# `$n` and serves the next read from it. `Type/IO/Handle.rakudoc`'s second
# "Creating Custom Handles" example depends on it: its `READ` ignores the byte
# count and returns the whole buffer every time, and rakudo still prints `one`
# then `two`.
#
# mutsu had no pushback, so the first `.get` swallowed every line at once, and
# `read_user_io_char` -- which asks for one byte at a time -- could not work
# against such a handle at all. Every expectation below was verified by running
# this file under real `raku`.

plan 9;

class Over is IO::Handle {
    has @.lines = ["one\n", "two\n", "three\n"];
    submethod TWEAK { self.encoding: 'utf8' }
    # Ignores its byte count and hands over everything at once.
    method READ(IO::Handle:D: Int:D \bytes --> Blob:D) {
        my $buf = Buf.new(@!lines.join.encode);
        @!lines = ();
        $buf
    }
    method EOF(IO::Handle:D: --> Bool:D) { @!lines.elems == 0 }
}

is Over.new.get, 'one', '.get takes only the first line, not the whole buffer';
# `.lines` on a custom handle does not chomp (measured: rakudo agrees).
is Over.new.lines.join('|'), "one\n|two\n|three\n",
    '.lines splits the over-returned buffer';
is Over.new.slurp, "one\ntwo\nthree\n", '.slurp gets all of it';
is Over.new.read(5).decode, "one\nt", '.read($n) honours $n';
is Over.new.getc, 'o', '.getc takes one character';

{
    my $h = Over.new;
    is $h.get, 'one', 'successive .get calls walk the pushback: first';
    is $h.get, 'two', '... second';
    is $h.get, 'three', '... third';
    is $h.get, Nil, '... and then Nil, without calling READ again';
}
