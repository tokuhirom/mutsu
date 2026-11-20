use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the parameters of a pointy
# block or a regex declaration with a `?` / `!` marker or a default
# (`$x?`, `Int $z = 1`, `:$y!`, `:$c = 42`): a parameter runs through its
# marker and its default.

plan 3;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }

my $b = Q[my $f = -> $x?, Int $z = 1, :$y!, :$c = 42 { };].AST.statements[0].expression.initializer.expression;
is $b.signature.parameters.map({ o($_) ~ '/' ~ o(.target) }).join(' '),
    '11-14/11-13 16-26/20-22 28-32/29-31 34-42/35-37', 'markers and defaults';
is o($b.signature), '11-42', 'the signature';
my $g = Q[grammar G { token w(:$e = 42) { a } }].AST.statements[0].expression.body.body.statement-list.statements[0].expression;
is o($g.signature.parameters[0]) ~ ' ' ~ o($g.signature.parameters[0].default), '20-28 26-28',
    'a token parameter with a default';
