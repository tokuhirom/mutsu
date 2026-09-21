use v6;
use Test;

plan 6;

# #8904: an `is rw` positional parameter bound to a CALLER's `$!attr`
# expression did not write back unless the call target happened to be
# literally `self` -- and even then only by an unrelated accident (a sibling
# alias mechanism meant for a same-class sigilless attribute mistakenly wrote
# through the CALLEE's own attribute cell). Three shapes:
#
#   self.chop($!buffer)              -- receiver IS the caller's self
#   $helper.chop($!buffer)           -- receiver is an unrelated object with
#                                        no such attribute: the write was lost
#   $other.chop($!buffer)            -- receiver is a DIFFERENT instance of
#                                        the SAME class: the write corrupted
#                                        the callee's own same-named attribute
#                                        instead
#
# All three must alias the CALLER's actual attribute storage through a
# shared container, exactly like an `is rw` parameter bound to a plain
# lexical variable already does.

class Helper {
    method chop(Buf $input is rw) {
        $input .= subbuf(1);
    }
}

class Reader {
    method peek(Buf $x) {
        return $x.elems;
    }
}

class Foo {
    has Buf $.buffer = Buf.new(1, 2, 3, 4, 5);

    method chop-self(Buf $input is rw) {
        $input .= subbuf(1);
    }

    method via-self() {
        self.chop-self($!buffer);
    }

    method via-helper(Helper $helper) {
        $helper.chop($!buffer);
    }

    method steal(Foo $other) {
        $other.chop-self($!buffer);
    }

    method peek-via(Reader $r) {
        return $r.peek($!buffer);
    }
}

my $a = Foo.new;
$a.via-self;
is $a.buffer.raku, 'Buf.new(2,3,4,5)',
    'is-rw arg aliasing self.method($!attr) still writes back';

my $b = Foo.new;
$b.via-helper(Helper.new);
is $b.buffer.raku, 'Buf.new(2,3,4,5)',
    'is-rw arg aliasing $other.method($!attr) writes back to the caller';

my $c = Foo.new;
my $d = Foo.new(buffer => Buf.new(9, 9, 9));
$c.steal($d);
is $c.buffer.raku, 'Buf.new(2,3,4,5)',
    'is-rw arg aliasing $peer.method($!attr) (same class) writes back to the caller';
is $d.buffer.raku, 'Buf.new(9,9,9)',
    '...and leaves the unrelated callee-side same-named attribute untouched';

# A non-`is rw` positional parameter must keep seeing (and leaving
# unaffected) a plain value -- the new container-promotion path must not
# leak into the common case.
my $e = Foo.new;
is $e.peek-via(Reader.new), 5,
    'a non-rw positional param still reads a plain $!attr value';
is $e.buffer.raku, 'Buf.new(1,2,3,4,5)',
    '...and a non-rw call leaves the attribute unchanged';
