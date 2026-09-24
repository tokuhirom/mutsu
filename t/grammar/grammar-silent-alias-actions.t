use Test;

plan 1;

grammar SilentAliasActions {
    rule TOP { $<units:unknown>=<.Ident> }
    rule Ident { <.alpha>+ }
}

class Actions {
    method Ident($/) { make ~$/.lc }
    method TOP($/) { make $<units:unknown>.ast }
}

is SilentAliasActions.subparse('Furlongs', :actions(Actions.new)).ast,
    'furlongs',
    'an alias around a silent subrule keeps its nested action result';
