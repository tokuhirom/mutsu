use v6;
use MONKEY-SEE-NO-EVAL;

# GH #11399: the method form `$code.EVAL` reads the caller's lexicals like
# the `EVAL $code` call does. A top-level `my` variable lives in a frame slot
# that is mirrored by name only when the chunk is known to reach a lexical by
# a dynamic name; the method-call spelling of EVAL did not count, so it read
# `Any` -- unless an unrelated `use` (a module with its own EVAL, such as
# Test) had latched the process-wide flag. So this file loads no module and
# prints its TAP by hand.

my $code = 42;
my @list = 1, 2, 3;
my $src = Q{grammar { token TOP { \d+ } }};
my $pkg = 'Internal::Abc';

my @checks = (
    (Q{ $code }.EVAL // 'undef') eq '42',
        'Q{ $x }.EVAL reads a caller lexical',
    (Q{ my $c; { $c = $code }; $c }.EVAL // 'undef') eq '42',
        'from a block inside the EVAL',
    (Q{ my $c; module M1 { $c = $code }; $c }.EVAL // 'undef') eq '42',
        'from a module body inside the EVAL',
    Q{ @list.elems }.EVAL == 3,
        'a caller array',
);

my $nested = qq:to/END/.EVAL;
my \$compiled;
module $pkg \{ \$compiled = \$src.EVAL \}
\$compiled
END
@checks.push: $nested ~~ Grammar, 'a nested EVAL inside an EVAL\'d module builds the grammar';
@checks.push: ($nested ~~ Grammar && ~$nested.parse('123') eq '123'), 'and the grammar parses';

say "1..{@checks / 2}";
for @checks.pairs.grep(*.key %% 2).kv -> $i, $p {
    say ($p.value ?? 'ok ' !! 'not ok ') ~ ($i + 1) ~ ' - ' ~ @checks[$p.key + 1];
}
