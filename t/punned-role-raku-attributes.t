use Test;

plan 3;

role Tc { has $.a; }

my $unset;
is Tc.new(:a($unset)).raku, 'Tc.new(a => Any)',
    'a punned role renders an undefined public attribute';

is Tc.new(a => 5).raku, 'Tc.new(a => 5)',
    'a punned role renders a defined public attribute';

is EVAL('role Te { has $.a }; my $a; Te.new(:$a).raku'), 'Te.new(a => Any)',
    'a punned role in EVAL retains its attribute metadata for raku';
