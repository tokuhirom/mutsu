use Test;

plan 6;

# `(my $x := EXPR)` binds `$x` straight to the value, so it owns no Scalar
# container and `$x =:= IterationEnd` holds -- also when the declaration
# has no local slot (inside a routine, a block, an `until` condition).
my $empty := ().iterator;

sub in-sub() { (my $l := $empty.pull-one); $l =:= IterationEnd }
ok in-sub(), 'parenthesized := declaration inside a sub';

{
    (my $l := IterationEnd);
    ok $l =:= IterationEnd, 'parenthesized := declaration inside a block';
}

sub until-loop() {
    1 until (my $line := $empty.pull-one) =:= IterationEnd;
    $line =:= IterationEnd
}
ok until-loop(), 'declaration in an until condition is seen after the loop';

my $it := <foo bar baz>.iterator;
sub pull() {
    my int $nr;
    ++$nr until (my $line := $it.pull-one) =:= IterationEnd
             || $line.contains('a');
    $line =:= IterationEnd ?? IterationEnd !! "$nr:$line"
}
is-deeply (pull(), pull()), ('1:bar', '0:baz'), 'pull-one style loop finds both lines';
ok pull() =:= IterationEnd, 'and then reports IterationEnd';

sub redeclared() { (my $p = 42); $p.VAR.^name }
is redeclared(), 'Scalar', 'an assigned declaration still owns a Scalar';
