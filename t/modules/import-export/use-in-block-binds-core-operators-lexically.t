use Test;

# ADR-8751 stage 2: the user candidates of a core operator whose op asks no
# user candidate (the bitwise operators, the ranges, `=>`, `===`, the
# junction constructors, `but`, `unicmp`, `minmax`, ...) bind in the lexical
# import set of the block whose `use` imports them. Inside the block the
# imported dispatcher replaces the op, and falls back to the core operator
# for operands no candidate accepts; a callee written outside the block does
# not see the candidates, and a closure taken in the block keeps them.
plan 9;

use lib 't/lib';

my $c = do { use UseInBlockCoreOpLexical; Cop.new };

# Each operator applied to two `Cop`s, written outside the block.
my @outside = (
    '%%'     => sub ($a, $b) { $a %% $b },
    '+&'     => sub ($a, $b) { $a +& $b },
    '+<'     => sub ($a, $b) { $a +< $b },
    '~^'     => sub ($a, $b) { $a ~^ $b },
    '~|'     => sub ($a, $b) { $a ~| $b },
    '?|'     => sub ($a, $b) { $a ?| $b },
    '..'     => sub ($a, $b) { $a .. $b },
    '^..^'   => sub ($a, $b) { $a ^..^ $b },
    '<=>'    => sub ($a, $b) { $a <=> $b },
    '==='    => sub ($a, $b) { $a === $b },
    '=~='    => sub ($a, $b) { $a =~= $b },
    '=>'     => sub ($a, $b) { $a => $b },
    '|'      => sub ($a, $b) { $a | $b },
    'before' => sub ($a, $b) { $a before $b },
    'but'    => sub ($a, $b) { $a but $b },
    'unicmp' => sub ($a, $b) { $a unicmp $b },
    'minmax' => sub ($a, $b) { $a minmax $b },
    '^'      => sub ($a, $) { ^$a },
    '+^'     => sub ($a, $) { +^$a },
    '?^'     => sub ($a, $) { ?^$a },
);

sub leaked(@ops) {
    # Only a Str is compared: a core Range of two objects never ends when
    # stringified.
    @ops.grep(-> $p {
        my $r = try $p.value.($c, $c);
        $r ~~ Str && $r eq "C:{$p.key}"
    })».key
}

{
    use UseInBlockCoreOpLexical;
    my @inside = (
        '%%'     => { $c %% $c },
        '+&'     => { $c +& $c },
        '+<'     => { $c +< $c },
        '~^'     => { $c ~^ $c },
        '~|'     => { $c ~| $c },
        '?|'     => { $c ?| $c },
        '..'     => { $c .. $c },
        '^..^'   => { $c ^..^ $c },
        '<=>'    => { $c <=> $c },
        '==='    => { $c === $c },
        '=~='    => { $c =~= $c },
        '=>'     => -> { $c => $c },
        '|'      => { $c | $c },
        'before' => { $c before $c },
        'but'    => { $c but $c },
        'unicmp' => { $c unicmp $c },
        'minmax' => { $c minmax $c },
        '^'      => { ^$c },
        '+^'     => { +^$c },
        '?^'     => { ?^$c },
    );
    is @inside.grep({ my $r = .value.(); !($r ~~ Str && $r eq "C:{.key}") })».key.join(' '), '',
        'inside the block every imported candidate answers its operator';

    is-deeply (1 .. 3, 1 ^..^ 4, 5 +& 3, 1 +< 3, 6 %% 3, 1 <=> 2).List,
        (1..3, 1^..^4, 1, 8, True, Less),
        'operands no candidate accepts get the core operator';
    is-deeply ('a' => 1, 1 === 1, 'a' unicmp 'b', (1 minmax 3)).List,
        (a => 1, True, Less, 1..3),
        'so do the pair, identity, unicmp and minmax operators';
    is (1 | 2).^name, 'Junction', 'the junction constructor falls back to the core';
    is (1 but True).so, True, 'but falls back to the core mixin';
    is-deeply (^3, +^3, ?^1).List, (^3, -4, False),
        'the prefixes ^, +^ and ?^ fall back to the core';

    is leaked(@outside).join(' '), '',
        'a callee written outside the block does not see the candidates';
}

my &keep = do { use UseInBlockCoreOpLexical; -> $a, $b { ($a +& $b) ~ ($a .. $b) } };
is keep($c, $c), 'C:+&C:..', 'a closure taken in the block keeps the candidates';
is leaked(@outside).join(' '), '', 'after the block the candidates are gone';
