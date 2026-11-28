use Test;

plan 4;

# A built-in method object found on a parent runs that method's own body;
# invoking it must not re-dispatch by name to the subclass override (#19298).
my $calls = 0;
grammar G {
    token TOP { \w+ }
    method subparse($s, *%a) { $calls++; nextwith($s, |%a) }
}

my $m = G.^mro[1].^find_method('subparse')(G, 'abc');
is $m.Str, 'abc', 'parent subparse object parses';
is $calls, 0, 'the override did not run';

my $r = G.subparse('abc');
is $r.Str, 'abc', 'direct call still reaches the override';
is $calls, 1, 'the override ran for the direct call';
