use Test;
use lib 't/lib';
use LvalueWritebackUnitState;

# #11275: `$outer.attr = v` inside a sub writes its target back by name. When
# the sub's `$outer` is a file-scope lexical it captured, that write must land
# in the captured variable, never in a same-named lexical of whatever frame is
# calling the sub.

plan 13;

class St { has $.top is rw = 0; }

my $state = St.new;
sub set-it { $state.top = 5; }
sub make-p(&cb) {
    my $state = 'b-state';
    my sub step($x) { cb($x); $state }
    &step
}
my &p = make-p(-> $x { set-it() });
is p(1), 'b-state', 'rw-accessor store does not clobber the caller lexical';
is $state.top, 5, '... and reaches the captured file-scope instance';

my $s = "hello";
my $v = 1;
my $pair = (a => $v);
my @arr = St.new, St.new;
my $inst = St.new;
sub w-substr { $s.substr-rw(0, 1) = "J"; }
sub w-pair   { $pair.value = 42; }
sub w-index  { @arr[0].top = 7; }
sub w-inst   { $inst.top = 9; }
sub run-in(&cb) {
    my $s = 'mine-s';
    my $pair = 'mine-p';
    my @arr = 1, 2;
    my $inst = 'mine-i';
    my sub step { cb(); "$s $pair @arr[] $inst" }
    step()
}
is run-in(-> { w-substr(); w-pair(); w-index(); w-inst() }),
    'mine-s mine-p 1 2 mine-i',
    'no lvalue writeback form leaks into the calling frame';
is $s, 'Jello', 'substr-rw store reaches the captured string';
is $pair.value, 42, 'Pair.value store reaches the captured pair';
is @arr[0].top, 7, 'indexed rw-accessor store reaches the captured array';
is $inst.top, 9, 'rw-accessor store reaches the captured instance';

# A routine's own local of the same name is still its own variable.
sub own-local {
    my $s = "world";
    $s.substr-rw(0, 1) = "W";
    $s
}
is run-in(-> { is own-local(), 'World', 'a local target is written in place' }),
    'mine-s mine-p 1 2 mine-i', '... without touching the caller either';
is $s, 'Jello', '... or the file-scope lexical of the same name';

# The Terminal::UI shape: a module's file-scope `$state`, reached through a
# callback from a closure that keeps its own `my $state`.
sub make-parser(&emit) { my $state = 0; -> $x { emit($x); ++$state } }
my &parse = make-parser(-> $x { set-top($x) });
is parse(3), 1, 'module rw-accessor store does not clobber the closure lexical';
is parse(4), 2, '... on a second call either';
is get-top(), 4, '... and reaches the module file-scope instance';
