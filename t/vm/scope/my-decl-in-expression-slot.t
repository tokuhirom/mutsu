use nqp;
use Test;

# A scalar declared in expression position -- `(my int $j = $i)` inside an
# `nqp::stmts`, `(my $x = ...)` as a statement -- is an ordinary lexical of the
# enclosing frame: later reads and stores of the name see it, a loop keeps
# updating it, closures capture it, and a sibling frame's same-named variable
# is unaffected.

plan 12;

sub forward(@a, int $i) {
    nqp::stmts(
      (my int $j = $i),
      (my int $elems = @a.elems),
      nqp::while(
        nqp::islt_i(($j = nqp::add_i($j, 1)), $elems)
          && @a.AT-POS($j) == @a.AT-POS($i),
        nqp::null
      ),
      $j
    )
}
is forward([1, 2, 2, 2, 3], 1), 4, 'nqp::stmts declarations drive a while loop';
is forward([5, 5, 5], 0), 3, 'the loop can run off the end';

sub paren-decl($x) { (my $y = $x * 2); $y + 1 }
is paren-decl(5), 11, 'a parenthesised declaration is read by the next statement';

sub typed-wrap() { (my int8 $b = 200); $b }
is typed-wrap(), -56, 'a native-int expression declaration wraps its value';

sub decl-value() { my $r = (my $v = 7); $r + $v }
is decl-value(), 14, 'the declaration is also the expression value';

sub reassign() { (my $c = 1); $c = $c + 41; $c }
is reassign(), 42, 'a later assignment updates the declared variable';

sub closure-capture() {
    (my $n = 10);
    my &inc = { $n++ };
    inc(); inc();
    $n
}
is closure-capture(), 12, 'a closure captures the expression-declared variable';

sub per-call($v) { (my $seen = $v); $seen }
is per-call(1), 1, 'each call gets its own variable (first call)';
is per-call(2), 2, 'each call gets its own variable (second call)';

my $outer = 'outer';
sub shadows() { (my $outer = 'inner'); $outer }
is shadows(), 'inner', 'the declaration shadows a same-named outer variable';
is $outer, 'outer', 'and the outer variable is untouched';

sub in-loop() {
    my @r;
    for ^3 -> $i { (my $sq = $i * $i); @r.push($sq) }
    @r
}
is-deeply in-loop(), [0, 1, 4], 'a declaration in a loop body is fresh per iteration';
