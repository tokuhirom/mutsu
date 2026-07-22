use Test;

# A block-form `my regex/token/rule NAME { ... }` terminates the statement, so
# an `if`/`for`/`while` on the NEXT line starts a new statement rather than
# being absorbed as a postfix statement modifier. When it was absorbed, the
# declaration got re-parented into the conditional's then-branch, so the rule
# was not declared yet when the condition's own regex referenced `<NAME>` --
# which then fell back to a (nonexistent) `.NAME` method call on Match.

plan 8;

my regex num { \d+ }
if "count = 42" ~~ / <num> / {
    is ~$<num>, '42', 'a my-regex declared on the previous line is callable';
}

my token word { \w+ }
if "hello" ~~ / <word> / {
    is ~$<word>, 'hello', 'my token, same shape';
}

my rule pair { \w+ '=' \w+ }
if "a = b" ~~ / <pair> / {
    is ~$<pair>, 'a = b', 'my rule, same shape';
}

my regex ident { <[a..zA..Z_]> \w* }
if "count = 42" ~~ / <ident> \s* '=' \s* <num> / {
    is ~$<ident>, 'count', 'two named rules in one pattern (name)';
    is +$<num>, 42, 'two named rules in one pattern (value)';
}

# The declaration must run unconditionally: a False condition on the next line
# must not swallow it.
my regex vowel { <[aeiou]> }
if False {
    flunk 'unreachable';
}
ok "beta" ~~ / <vowel> /, 'the rule survives a False conditional on the next line';

my regex digit3 { \d ** 3 }
for 1..1 {
    ok "x123y" ~~ / <digit3> /, 'a following `for` line does not absorb the declaration';
}

# A genuine one-line statement modifier still works on ordinary statements.
my $n = 0;
$n = 5 if True;
is $n, 5, 'ordinary postfix if modifier still parses';

done-testing;
