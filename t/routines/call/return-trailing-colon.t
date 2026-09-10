use Test;

# `return $x:` is the indirect-method-call spelling `$x.return`, which returns
# `$x` from the enclosing routine — identical to `return $x`. Regression: mutsu
# choked on the bare trailing colon ("Confused"). raku accepts it (though a bare
# `$x:` statement is rejected by both). (Geo::Ellipsoid::Utils writes `return $x:`.)

plan 5;

sub same_line() { my $x = 42; return $x: }
is same_line(), 42, "return \$x: on the same line as the closing brace";

sub newline() {
    my $x = 7;
    return $x:
}
is newline(), 7, "return \$x: followed by newline then closing brace";

sub with_expr() {
    my @v = 1, 2, 3;
    return @v.sum:
}
is with_expr(), 6, "return EXPR: where EXPR is a method call";

# A `:` starting an adverb/pair is NOT consumed as a bare trailing colon.
sub literal_ret() { return 5 }
is literal_ret(), 5, "plain return still works (no regression)";

sub bare_ret() { return }
ok bare_ret() === Nil, "bare return still works (no regression)";
