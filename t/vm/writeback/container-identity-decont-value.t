use Test;

# Found via DB::Xoos t/01-sql.t: its `gen-quote` decides "identifier or
# placeholder" with `val =:= try val."{val.^name}"()`. A raw sigilless
# parameter bound to a `$` variable is that variable's Scalar, so it is never
# identical to a plain value; one bound to a literal is the value itself.

plan 8;

my $tv = 1;
sub via-self(\val)  { val =:= val.self }
sub via-dyn(\val)   { val =:= try val."{val.^name}"() }
sub via-lit(\val)   { val =:= 1 }

ok !via-self($tv), 'raw param bound to a $ variable is not its own .self';
ok via-self(1),    'raw param bound to a literal is its own .self';
ok !via-dyn($tv),  'dynamic-name method result is a value, not the Scalar';
ok via-dyn(1),     'dynamic-name method result on a literal is identical';
ok !via-lit($tv),  'Scalar-bound raw param is not identical to a literal';
ok via-lit(1),     'literal-bound raw param is identical to the literal';

my \x = 5;
ok x =:= 5, 'sigilless term bound to a value is that value';
my $y = 5;
my \z = $y;
ok !(z =:= 5), 'sigilless alias of a $ variable is not the bare value';
