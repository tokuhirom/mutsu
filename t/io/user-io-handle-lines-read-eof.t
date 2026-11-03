use v6.d;
use Test;

# A user `IO::Handle` subclass that supplies `READ` and learns of the end of
# the stream only from an empty `READ` (as Shell::DSL's pipe handle does):
# `lines` yields the lines and no extra empty one after the last newline
# (#15647).

plan 4;

class OneByte is IO::Handle {
    has @.bytes;
    has Bool $!end = False;
    method READ(Int:D \bytes --> Buf:D) {
        unless @!bytes { $!end = True; return Buf.new }
        Buf.new(@!bytes.splice(0, 1))
    }
    method EOF { $!end }
    method opened { True }
}

sub handle(Str:D $text) {
    my $h = OneByte.new(:bytes($text.encode.list));
    $h.encoding('utf8');
    $h
}

is-deeply handle("abc\ndef\nghi\n").lines.List, ("abc", "def", "ghi"), 'a trailing newline adds no empty line';
is-deeply handle("abc\ndef").lines.List, ("abc", "def"), 'no trailing newline';
is-deeply handle("").lines.List, (), 'an empty stream has no lines';
is-deeply handle("a\n\nb\n").lines.List, ("a", "", "b"), 'an empty line in the middle is kept';
