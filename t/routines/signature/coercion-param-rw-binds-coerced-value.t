use Test;

# A coercion-typed `is rw` / `is raw` parameter whose variable argument the
# coercion converts binds the coerced value, immutable; the caller's variable
# keeps its value. An argument that already has the target type still binds
# the caller's container. mutsu wrote the coerced value back into the caller
# (Red's `infix:<==>(Red::AST $a, Numeric(Str) $b is rw)` turned a captured
# '01' into 1, Red t/091-rakuast-compiler).

plan 7;

sub show(Numeric(Str) $b is rw) { $b.raku }
my $r = '01';
is show($r), '1', 'the parameter holds the coerced value';
is $r, '01', "the caller's variable keeps its value";

sub store(Int() $b is rw) { $b = 5 }
my $s = '1';
throws-like { store($s) }, Exception, message => /'immutable'/,
    'the coerced parameter is immutable';
is $s, '1', 'and the caller is untouched';

my $t = 1;
store($t);
is $t, 5, 'an argument of the target type still aliases the variable';

sub raw-kind(Int() $b is raw) { $b.VAR.^name }
my $u = '1';
is raw-kind($u), 'Int', 'a converted `is raw` argument is a value';
my $v = 1;
is raw-kind($v), 'Scalar', 'an unconverted one is the container';
