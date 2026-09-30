use Test;

# A bare word between two terms is an infix only when an `infix:<word>` is
# declared in scope or rakudo's CORE declares `&infix:<word>`. mutsu used to
# take ANY non-reserved word speculatively and call the routine of the same bare
# name at run time, so `(1,2) cross (3,4)` and `(1,2) join (3,4)` ran, and an
# unknown `1 foo 2` failed only once execution reached it. rakudo rejects all of
# them at compile time (#9918, ADR-0093 "Known remaining divergence").

plan 16;

# --- Core list-op subs are not infixes ---------------------------------------

for 'say (1,2) cross (3,4);', 'say (1,2) zip (3,4);', 'say (1,2) sum (3,4);',
    'say (1,2) roundrobin (3,4);', 'say (1,2) join (3,4);',
    'my $x = (1,2) cross (3,4);', '(1,2) cross (3,4);' -> $code {
    throws-like $code, X::Syntax::Confused, "compile error: $code",
        reason => 'Two terms in a row';
}

# --- A plain sub is not an infix either ---------------------------------------

throws-like 'sub foo($a, $b) { "F" }; say 1 foo 2;', X::Syntax::Confused,
    'a two-arg plain sub does not become an infix', reason => 'Two terms in a row';

# --- The error is raised at compile time, before anything runs ---------------

my $ran = False;
sub mark { $ran = True }
try EVAL 'mark(); my $x = 1 foo 2;';
ok $! ~~ X::Syntax::Confused, 'unknown word infix is X::Syntax::Confused';
nok $ran, 'no statement before the unknown word infix was executed';

# --- Declared and core infix words keep working ------------------------------

is (EVAL 'sub infix:<foo>($a, $b) { "F" }; 1 foo 2'), 'F', 'a declared infix:<word> still parses';
is (EVAL 'multi infix:<bar>($a, $b) { $a ~ $b }; 1 bar 2'), '12', 'a declared multi infix:<word> still parses';
is (EVAL 'my &infix:<same> = -> $a, $b { $a eq $b }; 1 same "1"'), True,
    'an infix bound through my &infix:<word> still parses';
is-deeply (EVAL '(1,2) minmax (3,4)'), 1..4, 'core minmax still parses';
is (EVAL '1 unicmp 2'), Less, 'core unicmp still parses';
is-deeply (EVAL 'my \x = 3; x + 1'), 4, 'a sigilless term is untouched';
