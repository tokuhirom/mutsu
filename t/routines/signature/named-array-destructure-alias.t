use Test;
plan 4;

sub f(:out([$out?, :pass($out_pass) = True]),
      :err([$err?, :pass($err_pass) = True])) {
    "$out,$err,$out_pass,$err_pass";
}
is f(:out([1]), :err([2])), '1,2,True,True', 'two array-destructured named params with pass aliases';
is f(:out([1, :pass(5)]), :err([2])), '1,2,5,True', 'nested alias takes the supplied named value';

sub h(:out([$o?, :$q = 7])) { "$o,$q" }
is h(:out([1])), '1,7', 'plain nested named default';

sub g(:a([$x, $y])) { "$x$y" }
is g(:a([3, 4])), '34', 'array-destructured named param';
