use v6;
use Test;

plan 3;

# Grammar action classes can legitimately use names that also have native
# numeric dispatchers. The action's Match argument must reach the user method
# unchanged; `exp` is the collision exposed by LaTeX::Grammar.
grammar G {
    token TOP { <exp> }
    token exp { <base> '^' <power> }
    token base { <[a..z]> }
    token power { <[0..9]> }
}

class Actions {
    method TOP($/)  { make $<exp>.made }
    method exp($/)  { make [$/.^name, $<base>.made, $<power>.made] }
    method base($/) { make ~$/ }
    method power($/) { make +$/ }
}

my $m = G.parse('x^2', :actions(Actions.new));
ok $m.defined, 'grammar action parse succeeds';
is-deeply $m.made, ['G', 'x', 2],
    'the action named exp receives its Match argument, not Numeric failure';
nok $m<exp> ~~ Positional,
    'a singular Match capture is not treated as a Positional value';
