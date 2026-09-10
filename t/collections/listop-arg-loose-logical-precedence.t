use Test;

plan 12;

# Listop / colon-method argument lists are a comma-list at listop precedence, so
# each element stops before the loose word-logical operators (`andthen`/`and`/
# `or`/`orelse`/`notandthen`): `f $x andthen $y` is `(f $x) andthen $y`, not
# `f($x andthen $y)`.
{
    my @log;
    sub f($x) { @log.push("f:$x"); $x }
    is (f 1 andthen 2), 2, 'f 1 andthen 2 === (f 1) andthen 2';

    @log = ();
    f 1 andthen @log.push('then');
    is-deeply @log, ['f:1', 'then'], 'andthen runs after the listop call, not inside its args';

    @log = ();
    f 0 orelse @log.push('else');
    is-deeply @log, ['f:0'], 'orelse short-circuits on a true listop result';
}

# `.method: args andthen ...` stops the colon-arg list at `andthen`.
{
    my class Bar { has $.a; has $.b }
    my $made = 'no';
    my Bar $x;
    $x .=new: :10a, :20b andthen ($made = 'yes');
    is-deeply $x, Bar.new(:10a, :20b), '.=new: :10a, :20b stops before andthen';
    is $made, 'yes', 'andthen branch ran after the .=new colon-arg list';
}

# Chained `.=new` / `notandthen` / `orelse` / `andthen` with mixed arg forms
# (regression coverage for roast S03-operators/inplace.t subtest 36).
{
    my Int $x1;
    $x1 notandthen .=new;
    is-deeply $x1, 0, 'notandthen .=new (no args)';

    my Int $x2;
    $x2 orelse .=new andthen .=new: 43;
    is-deeply $x2, 43, 'orelse .=new andthen .=new: 43';

    my class Meow { has $.a; has $.b }
    my Meow $x4;
    $x4 orelse .=new :42a :70b andthen .=new
      andthen .=new: :100b andthen .=new: :10a, :20b;
    is-deeply $x4, Meow.new(:10a, :20b), 'chain of different ops with .=new colon-args';
}

# Comma-separated colon-arg lists still collect every element.
{
    my @collected;
    sub g(*@a) { @collected = @a }
    g 1, 2, 3;
    is-deeply @collected, [1, 2, 3], 'comma-separated listop args still collected';
}

# `(X.self).=meth` === `X.=meth` (`.self` returns the invocant unchanged), so the
# `.=` writes back through the underlying variable.
{
    my @a = 'FOO';
    (@a.self).=lc;
    is-deeply @a, ['foo'], '(@a.self).=lc writes back through @a';
}
{
    my $s = 'foo';
    ($s.self).=uc;
    is $s, 'FOO', '($s.self).=uc writes back through $s';
}
{
    my $n = 3.7;
    ($n.self).=Int;
    is $n, 3, '($n.self).=Int writes back through $n';
}
