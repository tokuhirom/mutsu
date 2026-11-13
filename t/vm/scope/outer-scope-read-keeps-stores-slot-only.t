use v6;
use Test;

# A read of an enclosing scope that the compiler resolved -- `OUTER::<$x>`, or
# an enclosing routine's `@_` read from a block in its body -- is answered the
# way an `EVAL` is: the code that holds it, and the code that declares that
# code, keep their lexicals visible by name. It no longer switches every
# routine of the program to writing each store into env (#15786), and a
# chunk with no `for`/`given`/`when` does not mirror its `($x = ...)`
# assignment targets either. These pin that the reads still see the right
# values.

plan 12;

class C {
    has $!lock = Lock.new;
    method m(*@_) { $!lock.protect: { my @r; @r.push($_ * 2) for @_; @r } }
    method sum(*@_) { my $s = 0; { $s += $_ for @_ }; $s }
}
is-deeply C.new.m(1, 2, 3), [2, 4, 6], 'a block run by another routine reads the method @_';
is C.new.sum(4, 5, 6), 15, 'a bare block reads the method @_ and writes its lexical';

my $x = 10;
sub g { my $x = 20; { OUTER::<$x> } }
is g(), 20, 'OUTER::<$x> from a block names the enclosing routine scope';

sub outer-sub { my $v = 'outer'; my sub inner { OUTER::<$v> }; $v = 'changed'; inner() }
is outer-sub(), 'changed', 'OUTER:: from a nested routine sees a later store';

sub h(*@_) { my $n = 0; for 1..3 { $n += @_.elems }; $n }
is h(4, 5), 6, 'a loop body reads the routine @_';

# A routine with an assignment-expression loop and no topicalizer, called
# after the reads above were compiled.
sub scan(@a, $needle) {
    my int $i = @a.elems;
    while ($i = $i - 1) >= 0 && @a[$i] == $needle { }
    $i
}
is scan([1, 7, 7, 7], 7), 0, 'an assignment-expression loop counts down';
is scan([1, 7, 7, 3], 7), 3, 'and stops at the first mismatch';

# The assignment-expression tags that a topicalizer does read.
{
    my @seen;
    my @y;
    for (@y = 3, 4) -> $v { @seen.push($v) }
    is-deeply @seen, [3, 4], 'for over an assignment expression';
    my @w = 1, 2;
    $_ *= 10 for (@w = 5, 6);
    is-deeply @w, [50, 60], 'for over an assignment expression writes its elements';
}

# EVAL still sees a routine's slot-only lexicals.
sub e { my $local = 41; $local = $local + 1; EVAL '$local' }
is e(), 42, 'EVAL reads a routine lexical after a store';

# A closure created in a loop over an assignment expression keeps its value.
{
    my @subs;
    my $k = 0;
    while ($k = $k + 1) <= 3 { my $c = $k; @subs.push({ $c }) }
    is @subs.map({ $_() }).join(','), '1,2,3', 'closures capture per-iteration values';
}

my @args-seen;
sub with-block(*@_) { my $cb = { @args-seen = @_ }; $cb() }
with-block(1, 2);
is-deeply @args-seen, [1, 2], 'a closure called later reads the routine @_';
