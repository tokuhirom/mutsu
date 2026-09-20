use v6;
use Test;

# `Parameter`/`Signature` are ordinarily only materialized by the runtime
# from a real declaration, but Raku exposes both as constructible types too
# (`Parameter.new`, `Signature.new`) so code can synthesize a signature at
# runtime -- most commonly to reconstruct it back into declaration syntax via
# `.perl`/`.raku` and `EVAL` a fresh sub from it (the pattern the small
# ecosystem module Template::Classic uses). mutsu previously had no
# constructor for either type at all ("Unknown method value dispatch
# (fallback disabled): new on Parameter"). Found via the `Karabiner::
# CompModGenerator` zef distribution (locked/worked via the ecosystem
# distribution roulette, board #7884).

plan 8;

my $p1 = Parameter.new(:name('$foo'));
isa-ok $p1, Parameter, 'Parameter.new returns a Parameter';
is $p1.name, '$foo', 'Parameter.new(:name) sets the sigiled name';

my $p2 = Parameter.new(:name('@bar'));
is $p2.name, '@bar', 'Parameter.new(:name) preserves an @ sigil';

my @params = ('$a', '$b', '$c').map: { Parameter.new(:name($_)) };
my $sig = Signature.new(:@params, :returns(Int));
is $sig.raku, ':(Any $a, Any $b, Any $c --> Int)',
    'Signature.new(:@params, :returns) renders valid, EVAL-able declaration syntax'
    ~ ' (no spurious leading ";;", and rakudo shows every param\'s nominal type explicitly)';

my $sig-no-return = Signature.new(:@params);
is $sig-no-return.raku, ':(Any $a, Any $b, Any $c --> Mu)',
    'Signature.new with no :returns defaults to the explicit Mu return type';

# The actual downstream use: reconstruct the signature into a real `sub`
# via EVAL, exactly like Template::Classic's `template()` does, and confirm
# it accepts and binds the right number of positional arguments.
my &f = EVAL "sub {$sig.raku.substr(1)} \{ \$a + \$b + \$c \}";
is &f(1, 2, 3), 6, 'a sub EVALed from a constructed Signature binds positionally';

dies-ok { &f(1, 2) }, 'the EVALed sub still enforces the reconstructed arity';

# A Parameter pulled from ordinary introspection (not Parameter.new) still
# round-trips through Signature.new -- Signature.new must not assume every
# element of :@params came from Parameter.new itself.
sub existing($x, $y) {}
my @introspected = &existing.signature.params;
my $sig2 = Signature.new(:params(@introspected));
is $sig2.raku, ':(Any $x, Any $y --> Mu)',
    'Signature.new accepts introspected Parameters, not just freshly constructed ones';
