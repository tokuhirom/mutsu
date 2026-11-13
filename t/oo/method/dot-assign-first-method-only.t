use Test;

plan 10;

# `.=` mutates with ONE method call; a further postfix applies to the
# assignment's result (CSS::Stylesheet's `$stylesheet .= new(...).parse($css)`
# stores the new object and then parses into it).

class C { has @.r; method add($x) { @!r.push: $x; self } }

my $c = C.new.add(1);
$c .= new.add(2);
is-deeply $c.r, [2], '$c .= new.add(2) stores the new object, then adds to it';

my $d = C.new.add(9);
$d .= new().add(3);
is-deeply $d.r, [3], 'with an empty argument list';

my $x = 'ab';
$x .= uc.flip;
is $x, 'AB', 'only the first method is stored';

my $y = 'ab';
is ($y .= uc.flip), 'BA', 'the rest of the chain applies to the result';
is $y, 'AB', 'and leaves the variable alone';

my @a = 1, 2;
@a .= map(* * 2).sum;
is-deeply @a, [2, 4], 'an array keeps the mapped values, not the sum';

my %h = a => 'x';
%h<a> .= uc.flip;
is-deeply %h, {a => 'X'}, 'a hash element';

my $v = '20';
$v .= Numeric.Rat;
is $v.^name, 'Int', '.Rat is applied to the result, not stored';

$_ = 'q';
$_ .= uc.flip;
is $_, 'Q', 'the topic';

my $w = 'ab';
$w .= "uc"().flip;
is $w, 'AB', 'a quoted method name';
