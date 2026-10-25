use Test;

# A `where` inside a destructuring sub-signature may name the variables bound
# by that sub-signature: a `:key($var)` rename target, and (in a multi) the
# variables of an EARLIER parameter's sub-signature. Red's
# Red::AST::Optimizer::AND candidates are written this way
# (`GeGt $right (Red::AST :left($rl) where { compare $ll, $rl }, |)`), so none
# of them was ever selected (Red 0.2.7 t/06-optimizer).

plan 9;

role Node { has $.left; has $.right }
class Gt does Node {}
class Ge does Node {}
my subset GeGt of Node where Gt | Ge;
sub same($a, $b) { $a === $b }

my $col = 5;
my $x = Gt.new(left => $col, right => 1);
my $y = Ge.new(left => $col, right => 10);
my $z = Gt.new(left => 7, right => 10);

sub rename-where($n (Any :left($l) where { $l == 5 }, |)) { "rename $l" }
is rename-where($x), 'rename 5', 'a where names the rename target of its own parameter';
throws-like { rename-where($z) }, Exception, 'and rejects a value that fails it';

class Opt {
    multi method pick(GeGt $l (Any :left($ll), Any :right($lr), |),
                      GeGt $r (Any :left($rl) where { same $ll, $rl }, Any :right($rr), |), $) {
        "same-left $lr $rr"
    }
    multi method pick($, $, $) { 'fallback' }

    multi method lit(GeGt $l (Any :left($ll), |),
                     GeGt $r (Any :left($rl) where { $rl == 5 }, |), $) { 'literal' }
    multi method lit($, $, $) { 'fallback' }

    multi method both(GeGt $l (Any :left($ll), |),
                      GeGt $r (Any :left($rl) where { $ll == $rl }, |), $) { "both $ll $rl" }
    multi method both($, $, $) { 'fallback' }
}

is Opt.pick($x, $y, 1), 'same-left 1 10', 'a later where sees an earlier parameter\'s variable';
is Opt.pick($x, $z, 1), 'fallback', 'and the candidate is skipped when it fails';
is Opt.lit($x, $y, 1), 'literal', 'a where on a rename target in a multi';
is Opt.lit($x, $z, 1), 'fallback', 'a where that fails skips the candidate';
is Opt.both($x, $y, 1), 'both 5 5', 'a where comparing two parameters\' variables';
is Opt.both($z, $y, 1), 'fallback', 'and failing the comparison';
is Opt.pick(Ge.new(left => $col, right => 3), Ge.new(left => $col, right => 4), 1), 'same-left 3 4',
    'the subset type accepts either class';
