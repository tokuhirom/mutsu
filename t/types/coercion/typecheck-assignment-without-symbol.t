use Test;

# A hand-built X::TypeCheck::Assignment without `symbol` has a `.message` that
# dies in rakudo, so its `Str` falls back to "Something went wrong in (Assignment)"
# (Configuration's `throws-like ... message => /Assignment/`, #16047).
plan 7;

my $e = X::TypeCheck::Assignment.new(got => "bla", expected => Int);
dies-ok { $e.message }, '.message dies without a symbol';
is $e.Str, 'Something went wrong in (Assignment)', '.Str falls back to the class name';

my $thrown;
try { $e.throw; CATCH { default { $thrown = $_ } } }
isa-ok $thrown, X::TypeCheck::Assignment, 'it throws as itself';
is "$thrown", 'Something went wrong in (Assignment)', 'a thrown one stringifies to the fallback';
is "Error: $thrown", 'Error: Something went wrong in (Assignment)', 'and interpolates into a wrapped message';

my $with = X::TypeCheck::Assignment.new(got => "bla", expected => Int, symbol => '$x');
my $caught;
try { $with.throw; CATCH { default { $caught = .message } } }
is $caught, 'Type check failed in assignment to $x; expected Int but got Str ("bla")', 'with a symbol the thrown .message is the type-check text';
is $with.message, $caught, 'same text before throwing';
