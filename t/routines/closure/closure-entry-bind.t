use Test;

# The closure entry bind (#18267 stage 2): a sub called through a code
# variable binds plain `$` and sigilless `\x` / `Mu \x` positionals without
# the general binder when every argument reaches the one binder arm it
# reproduces. Each case is called in a loop through a `&`-variable.

plan 17;

class C { has $.x = 3 }
my $o = C.new;

my sub ga(Mu \obj, $attr) is raw { obj.x + $attr }
my &g = &ga;
is-deeply (^3).map({ g($o, $_) }).List, (3, 4, 5), 'an instance to a sigilless Mu parameter';

my sub raw-ro(\v) { v = 5 }
my &r = &raw-ro;
throws-like { r(3) }, X::Assignment::RO, 'a sigilless parameter bound to a value is read-only';
my $var = 10;
r($var);
is $var, 5, 'a sigilless parameter given a variable aliases it';

my sub mu(Mu \m) { m.^name }
my &m = &mu;
is-deeply (Mu, Int, $o).map({ m($_) }).List, <Mu Int C>, 'Mu \\x takes a type object, Mu, an instance';

my sub anyp(\a) { a.^name }
my &a = &anyp;
throws-like { a(Mu) }, X::TypeCheck::Binding::Parameter, 'an untyped sigilless parameter refuses Mu';
is a(Int), 'Int', 'an untyped sigilless parameter takes a type object';

my sub pl($x) { $x }
my &p = &pl;
my Int $typed = 4;
is p($typed), 4, 'a typed variable to a plain parameter';
is p($o).x, 3, 'an instance to a plain parameter';
throws-like { p(Mu) }, X::TypeCheck::Binding::Parameter, 'a plain parameter refuses Mu';

my @arr = 1, 2;
my sub pa($x) { $x.push(3) }
my &pa2 = &pa;
pa2(@arr);
is-deeply @arr, [1, 2, 3], 'an array variable to a plain parameter is shared';

my sub cb(\f) { f.() }
my &cb2 = &cb;
is cb2({ 42 }), 42, 'a sigilless parameter given a block';

my sub ro($x) { $x = 1 }
my &ro2 = &ro;
throws-like { ro2(5) }, X::AdHoc, 'a plain parameter is read-only', message => /readonly/;

my sub rawp($x is raw) { $x }
my &rawp2 = &rawp;
my $rv = 7;
is rawp2($rv), 7, 'an is raw parameter given a variable';

my sub two($a, $b) { "$a-$b" }
my &two2 = &two;
dies-ok { two2(1) }, 'too few arguments still die';
is-deeply (^3).map({ two2($_, $o.x) }).List, ('0-3', '1-3', '2-3'), 'two plain parameters';

my @cv = 1, 2, 3;
my &cw = sub (\t) { t = (6, 6) };
cw(@cv[1, 2]) for ^2;
is @cv.raku, '[1, 6, 6]', 'a slice to a sigilless parameter is written through';
my @cr = 1, 2, 3;
my &crw = sub ($t is raw) { $t = (5, 5) };
dies-ok { crw(@cr[0, 1]) }, 'an is raw $ parameter given a slice cannot be assigned a list';
