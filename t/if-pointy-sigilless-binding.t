use v6;
use Test;

# A sigilless binding in an `if` condition — `if my \r = EXPR {}` or the
# pointy `if EXPR -> \r {}` — binds the value ITSELF (no scalar container),
# so an `@`-array condition must not itemize: `r.Array` yields the same
# elements, not a single nested item. DBDish::StatementHandle's `row` is
# `if my \r = self._row { ... r.Array }`, whose result feeds `my @row = ...`.
plan 8;

class C {
    method _row { my @l; @l.push(1); @l.push(2); @l }
    method decl    { if my \r = self._row { r.Array } }
    method pointy  { if self._row -> \r { r.Array } }
    method with-p  { with self._row -> \r { r.Array } }
}

my @a = C.new.decl;
is @a.elems, 2, 'if my \r = @rows: r.Array keeps its elements';
is @a[0], 1, '... first element intact';

my @b = C.new.pointy;
is @b.elems, 2, 'if EXPR -> \r: r.Array keeps its elements';

my @c = C.new.with-p;
is @c.elems, 2, 'with EXPR -> \r: r.Array keeps its elements';

if 42 -> \v {
    is v, 42, 'pointy sigilless binds the condition value';
}

my $else-taken = False;
if 0 -> \w { } else { $else-taken = True }
ok $else-taken, 'falsy condition still takes the else branch';

# Scalar pointy binding stays a container.
if 5 -> $v { is $v, 5, 'scalar pointy binding still works' }

# Re-binding in a loop condition works per iteration.
my $i = 0;
my @seen;
while my \x = ($i < 3 ?? ++$i !! Nil) { @seen.push(x) }
is @seen.join(','), '1,2,3', 'sigilless while-condition binding re-binds each iteration';

done-testing;
