use Test;

# A `$^placeholder` used only inside a `[a;b]` multi-dimensional subscript
# must still make the enclosing bare block take that parameter. Found via
# Math::Matrix (`map { @!rows[$^i;$^i-$start] }, ^n`), where the block was
# compiled with arity 0 and every index read the same topic.

plan 5;

my @rows = [1,2],[3,4];
my $start = 0;

is-deeply (map { @rows[$^i;$^i-$start] }, ^2).list, (1, 4), 'placeholder in both dimensions';
is-deeply (map { @rows[$^i;1] }, ^2).list, (2, 4), 'placeholder in first dimension only';
is-deeply (map { @rows[1;$^j] }, ^2).list, (3, 4), 'placeholder in second dimension only';
is-deeply (map { @rows[$^i][$^i] }, ^2).list, (1, 4), 'chained subscripts (control)';

class C {
    has @!r = [1,2],[3,4];
    method diag($start = 0) { (map { @!r[$^i;$^i-$start] }, ^2).list }
}
is-deeply C.new.diag, (1, 4), 'inside a method over an attribute';
