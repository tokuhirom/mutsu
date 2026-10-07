use Test;

plan 4;

# #12205: a user `trait_mod:<is>` is applied to a parameter of a Signature
# literal when the literal is built, as it is for a `sub`'s parameters.
role Marked { method marked-by() { 'marked' } }
multi trait_mod:<is>(Parameter:D $param, :$tagged! --> Nil) { $param does Marked }

my $sig = :(Int $x is tagged);
is $sig.params[0].marked-by, 'marked', 'trait applied to a signature literal parameter';
is $sig.params[0].name, '$x', 'parameter is otherwise intact';
ok $sig ~~ Signature, 'the literal is still a Signature';

my $plain = :(Int $y);
nok $plain.params[0].^can('marked-by'), 'untagged literal parameter is not marked';
