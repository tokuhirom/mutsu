use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a `for` loop's destructuring
# parameter (`-> ($a, $b)`, `-> Pair (:key($k))`) spans its pattern, type
# included; its `sub-signature` the text inside the brackets; each inner
# parameter its own text, and a renamed one (`:key($k)`) targets its variable.

plan 7;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }
sub params($sig) {
    $sig.parameters.map({ o($_) ~ '/' ~ (.target.DEFINITE ?? o(.target) !! '-') }).join(' ')
}
sub shape($src) {
    my $sig = $src.AST.statements[1].body.signature;
    my @out = o($sig);
    for $sig.parameters -> $p {
        @out.push: o($p) ~ ' ' ~ o($p.type // Nil);
        @out.push: '[' ~ o($p.sub-signature) ~ ' ' ~ params($p.sub-signature) ~ ']' if $p.sub-signature;
    }
    @out.join(' ')
}

is shape(Q[my @l; for @l -> ($a, $b) { }]), '17-25 17-25 - [18-24 18-20/18-20 22-24/22-24]', 'a parenthesised pattern';
is shape(Q[my @l; for @l -> [$a, $b] { }]), '17-25 17-25 - [18-24 18-20/18-20 22-24/22-24]', 'a bracketed pattern';
is shape(Q[my @l; for @l -> List ($a, $b) { }]), '17-30 17-30 17-21 [23-29 23-25/23-25 27-29/27-29]', 'a typed pattern';
is shape(Q[my @l; for @l -> ($a, $b), ($c, $d) { }]), '17-35 17-25 - [18-24 18-20/18-20 22-24/22-24] 27-35 - [28-34 28-30/28-30 32-34/32-34]', 'two patterns';
is shape(Q[my @l; for @l -> ($a, ($b, $c)) { }]), '17-31 17-31 - [18-30 18-20/18-20 22-30/-]', 'a nested pattern';
is shape(Q[my @l; for @l -> Pair (:key($k), :value(:$v)) { }]), '17-45 17-45 17-21 [23-44 23-31/28-30 33-44/41-43]', 'renamed named parameters';
is shape(Q[my @l; for @l -> Pair $p (:$key) { }]), '17-32 17-32 17-21 [26-31 26-31/27-31]', 'the sub-signature of a named parameter';
