use Test;

plan 5;

sub last($x) { "last $x" }
is last(3), 'last 3', 'a user sub named last handles its parenthesized call';

sub next($x) { "next $x" }
is next(4), 'next 4', 'a user sub named next handles its parenthesized call';

sub redo($x) { "redo $x" }
is redo(5), 'redo 5', 'a user sub named redo handles its parenthesized call';

sub proceed($x) { "proceed $x" }
is proceed(6), 'proceed 6', 'a user sub named proceed handles its parenthesized call';

sub return($x) { "return $x" }
is return(7), 'return 7', 'a user sub named return handles its parenthesized call';
