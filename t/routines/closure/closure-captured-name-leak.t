use v6;
use Test;

plan 4;

# A method-local `my $x` must not receive the value of the MAINLINE's
# closure-captured `$x` just because the method called a mainline closure.
#
# Mechanism (the Text::CSV `$io-in` leak, 90_csv.t test 495): a mainline sub
# capturing `$x` boxes it into a shared cell; every later mainline closure's
# captured env then carries `x => <cell>`, force-installed into the callee
# frame at entry. The closure-exit env writeback used to treat that installed
# binding as "a mutation the caller must observe" whenever the caller's env
# happened to contain the same bare name — e.g. a method whose own `my $x`
# was mirrored into env by a smartmatch's locals sync — and clobbered the
# method's unrelated local with the mainline value.

class Leak {
    method go(&cb) {
        my $x;
        # A smartmatch mirrors the frame's locals into env by name, putting
        # this frame's `x` entry where the exit writeback can see it (the same
        # shape as Text::CSV's `$io-in ~~ IO::Handle` gates).
        "y" ~~ /y/;
        my @r;
        @r.push: cb();
        @r.push: cb();
        is $x.defined, False, 'method-local $x stays undefined after calling a mainline closure';
        @r;
    }
}

my $x = 42;
sub touch { $x.say }          # never called; its capture boxes mainline $x

my int $i = 0;
my $rows = [10, 20];
sub getrow { return $rows[$i++]; }

my @got = Leak.go(&getrow);
is-deeply @got, [10, 20], 'closure still returns its own captured data';
is $x, 42, 'mainline $x unchanged';
is $i, 2, 'closure free-var writes still propagate to the mainline';
