use Test;
use MONKEY-SEE-NO-EVAL;

# Resolving a parameterized role's candidate binds its parameters in a trial
# scope; their readonly marks must not stay on the resolving scope's
# same-named lexicals (#10494).

plan 3;

my $a = 0;
role ParamA[:$a = 1] { method pa { $a } }
my $c = EVAL 'class ComposesA does ParamA[:a(7)] { }; ComposesA';
is $c.new.pa, 7, 'the role parameter is bound in the composed method';
lives-ok { $a = 3 }, 'the same-named outer lexical is still writable';
is $a, 3, 'and holds the assigned value';
