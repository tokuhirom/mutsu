use Test;

# #13055: a quoted pseudo-method call on a variable (`$x."WHICH"()`) answers
# like the bareword form unless the class declares a method of that name.

plan 11;

my $x = 5;
is $x."WHICH"(), 'Int|5', '$x."WHICH"()';
is $x."WHERE"().^name, 'Int', '$x."WHERE"()';
is $x."DEFINITE"(), True, '$x."DEFINITE"()';
ok $x."WHAT"() === Int, '$x."WHAT"()';
my $m = 'WHICH';
is $x."$m"(), 'Int|5', 'a method name held in a variable';
my @a = 1, 2;
is @a."DEFINITE"(), True, 'an array variable';

class C { method WHAT { 'user WHAT' }; method WHICH { 'user WHICH' } }
my $c = C.new;
is $c."WHAT"(), 'user WHAT', 'a quoted call reaches a user WHAT';
is $c.WHAT.^name, 'C', 'the bareword WHAT stays the macro';
is $c."WHICH"(), 'user WHICH', 'a quoted call reaches a user WHICH';
my $plain = C.new;
ok $plain."DEFINITE"(), 'DEFINITE of an instance with no DEFINITE of its own';

my $v = 3;
is $v."VAR"().^name, 'Scalar', '$v."VAR"() is the container';
