use Test;

plan 6;

# `Signature.params` used to build a fresh `Parameter` on every access, so a
# mixin applied to one materialization vanished on the next read, and
# `$sig.params[0] === $sig.params[0]` was False.
# todo/tickets/parameter-objects-have-no-stable-identity.md

role Q { }

sub f(:$mp) { }
my $p = &f.signature.params[0];
$p does Q;
ok $p ~~ Q, 'does mutates the object';
ok &f.signature.params[0] ~~ Q, 'the mixin persists across a fresh &f.signature read';
ok &f.signature.params[0] === &f.signature.params[0], 'repeated .params reads share identity';

# An anonymous closure held in a variable (no bareword re-lookup involved)
# already had a stable SubData, so this is a narrower check on the same code
# path.
my $g = -> :$gp { };
my $gparam = $g.signature.params[0];
$gparam does Q;
ok $g.signature.params[0] ~~ Q, 'a closure held in a variable keeps the mixin too';

# Different declarations, and different `multi` candidates of the same name,
# must never share a cache entry.
sub h1(:$a) { }
sub h2(:$b) { }
is &h1.signature.params[0].name, '$a', 'an unrelated sub keeps its own params';
is &h2.signature.params[0].name, '$b', 'and does not collide with a same-shaped sibling';
