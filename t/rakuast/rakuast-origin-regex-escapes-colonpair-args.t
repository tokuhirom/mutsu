use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a regex literal written with
# backslash-escaped characters (`a\@b`), and the colonpair arguments of a
# regex call assertion (`<word(:a(1), :b, :!c, :d<x>)>`) with their values.

plan 3;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }
sub body(Str $s) { $s.AST.statements[0].expression.initializer.expression.body }

is o(body(Q[my $r = /a\@b/;])), '9-13', 'an escaped character in a literal';
my $args = body(Q[my $r = /<word(:a(1), :b, :!c, :d<x>)>/;]).args;
is $args.args.map({ .^name.split('::').tail(2).join('::') ~ '@' ~ o($_) }).join(' '),
    'ColonPair::Value@15-20 ColonPair::True@22-24 ColonPair::False@26-29 ColonPair::Value@31-36',
    'colonpair arguments';
is o($args.args[0].value) ~ ' ' ~ o($args.args[3].value), '17-20 33-36', 'their values';
