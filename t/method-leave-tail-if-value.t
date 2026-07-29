use v6;
use Test;

# A method carrying a LEAVE (or other block phaser) must still produce the
# value of a tail `if` / block / declaration — DBDish::Pg's `prepare` ends in
# `LEAVE { … }; if $result && $result.is-ok { StatementHandle.new(…) } else …`
# and mutsu returned Nil from it (the phaser-carrying compile path lacked the
# value-position statement forms the plain path reifies).
plan 6;

my $left = 0;
class C {
    method tail-if {
        LEAVE { $left++ }
        if 1 { 42 } else { 'no' }
    }
    method tail-if-else {
        LEAVE { $left++ }
        if 0 { 'no' } else { 'e' }
    }
    method tail-block {
        LEAVE { $left++ }
        do { 'blk' }
    }
    method tail-decl {
        LEAVE { $left++ }
        my $x = 7;
        $x
    }
}

is C.new.tail-if, 42, 'LEAVE + tail if (then branch) returns its value';
is C.new.tail-if-else, 'e', 'LEAVE + tail if (else branch) returns its value';
is C.new.tail-block, 'blk', 'LEAVE + tail do-block returns its value';
is C.new.tail-decl, 7, 'LEAVE + tail variable read returns its value';
is $left, 4, 'every LEAVE phaser ran';

# sub form stays correct too (guards the shared compile helper).
sub s-tail-if { LEAVE { $left++ }; if 1 { 'sub-ok' } }
is s-tail-if(), 'sub-ok', 'sub LEAVE + tail if keeps its value';

done-testing;
