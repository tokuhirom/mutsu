# `.parse` runs its start rule on `self.new` — a BUILT instance, with
# `= default` values, BUILD and TWEAK applied — while every subrule runs on a
# cursor minted without BUILD (#10848). So `self` in a code block of the start
# rule's own body reads the grammar's defaults, a subrule's code block reads
# uninitialised attributes, and a method start rule's `self` is built too.
# Each case runs on the compiled engine and on the tree walk
# (`MUTSU_RX_VM=off`); both must print rakudo's values.
use Test;

my @cases =
    'start rule code block sees the default; a subrule does not',
        'grammar G { has $.n = 1; token TOP { <t> { print "top ", self.n, ";" } }; token t { a { print "t ", self.n.raku, ";" } } }; G.parse("a"); say ""',
        't Any;top 1;',
    'an instance invocant is not reused: the start rule gets a fresh new',
        'grammar G { has $.n = 1; token TOP { a { say self.n } } }; G.new(n => 7).parse("a")',
        '1',
    'a method start rule runs on the built instance',
        'grammar M { has $.n = 1; method TOP { say self.n; self.new.match("a") } }; try M.parse("a")',
        '1',
    'a required attribute dies at .parse',
        'grammar R { has $.x is required; token TOP { a } }; R.parse("a"); CATCH { default { say .^name } }',
        'X::Attribute::Required',
    'TWEAK runs once per parse; the invocant stays at the start position',
        'grammar T { has $.n = 1; submethod TWEAK { print "tweak;" }; token TOP { <t> { print self.pos, ";" } }; token t { a } }; T.parse("a"); T.parse("a"); say ""',
        'tweak;0;tweak;0;',
    'a parse inside a code block does not leak its invocant outward',
        'grammar I { has $.k = "inner"; token TOP { b { print self.k, ";" } } }; grammar O { has $.k = "outer"; token TOP { <t> { print self.k, ";" } }; token t { a { I.parse("b"); print self.k // "Any", ";" } } }; O.parse("a"); say ""',
        'inner;Any;outer;';

plan @cases / 3 * 2;

for @cases -> $name, $code, $expected {
    for <on off> -> $engine {
        my %env = %*ENV;
        %env<MUTSU_RX_VM> = $engine;
        my $proc = run($*EXECUTABLE, '-e', $code, :out, :err, :%env);
        is $proc.out.slurp(:close).trim, $expected, "$name (compiled engine $engine)";
        $proc.err.slurp(:close);
    }
}
