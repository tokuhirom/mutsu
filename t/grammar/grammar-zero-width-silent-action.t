use v6;
use Test;

plan 1;

class Actions {
    has @.warnings;
    method end-block($/) {
        @!warnings.push($<closing-paren> ?? 'closed' !! 'missing');
    }
}

grammar Recovery {
    rule TOP { '{' 'x' <.end-block> }
    rule end-block {[$<closing-paren>='}' ';'?]?}
}

my $actions = Actions.new;
Recovery.subparse('{x', :$actions);
is $actions.warnings, <missing>,
    'a zero-width silent subrule still dispatches its action';
