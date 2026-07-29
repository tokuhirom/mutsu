use v6;
use Test;

# Action methods must fire for subrules matched INSIDE a positional capture
# group, and their `.made` must be readable through `$0[…]`. DBDish::Pg's
# placeholder tokenizer is exactly this shape:
#   token TOP { ^ ( <normal> | <placeholder> )* $ }
# with per-token action methods joined in the TOP action.
plan 4;

my grammar Tok {
    token normal      { <-[?]>+ }
    token placeholder { '?' }
    token TOP { ^ ( | <normal> | <placeholder> )* $ }
}

my class Actions {
    has $.counter = 0;
    method normal($/)      { make $/.Str }
    method placeholder($/) { make '$' ~ ++$!counter }
    method TOP($/) {
        make $0.flatmap({ .values[0].ast }).join;
    }
}

is Tok.parse('ab?cd?e', :actions(Actions.new)).ast, 'ab$1cd$2e',
    'subrule actions fire inside a quantified positional group';

is Tok.parse('no placeholders', :actions(Actions.new)).ast, 'no placeholders',
    'single-alternative group round-trips';

# Non-quantified single group: the same walk must recurse into it.
my grammar One {
    token word { \w+ }
    token TOP { ^ (<word>) '!' $ }
}
my class OneActions {
    method word($/) { make $/.Str.uc }
    method TOP($/)  { make $0<word>.ast }
}
is One.parse('hey!', :actions(OneActions.new)).ast, 'HEY',
    'action fires for a subrule inside a plain group';

# A named subrule OUTSIDE a group must keep firing exactly once (no
# double-dispatch through the positional walk).
my grammar Count {
    token item { \w+ }
    token TOP { ^ <item>+ % ',' $ }
}
my class CountActions {
    has $.fired = 0;
    method item($/) { $!fired++ }
    method TOP($/)  { make $!fired }
}
is Count.parse('a,b,c', :actions(CountActions.new)).ast, 3,
    'named subrule actions fire exactly once each';

done-testing;
