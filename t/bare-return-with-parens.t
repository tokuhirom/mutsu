use Test;

# `return()` -- an argument list attached with NO space -- is a zero-argument
# call and returns Nil, exactly like a bare `return`. `return ()` (with a space)
# is a different thing: the `()` is a term, an empty list, passed as the
# argument, so the routine returns `()`.
#
# Verified assertion-for-assertion against rakudo.

plan 17;

# --- the zero-argument call -----------------------------------------------
sub bare        { return }
sub parens      { return() }
sub parens-ws   { return( ) }
sub bare-mod    { return if 1; 42 }
sub parens-mod  { return() if 1; 42 }
sub parens-nl   {
    return()
}
sub bare-dead   { return; 1 }

is-deeply bare(),       Nil, 'a bare `return` returns Nil';
is-deeply parens(),     Nil, '`return()` returns Nil';
is-deeply parens-ws(),  Nil, '`return( )` returns Nil';
is-deeply bare-mod(),   Nil, '`return if 1` returns Nil';
is-deeply parens-mod(), Nil, '`return() if 1` returns Nil';
is-deeply parens-nl(),  Nil, '`return()` before a newline returns Nil';
is-deeply bare-dead(),  Nil, '`return; 1` returns Nil, not 1';

# --- a space makes the `()` an argument -----------------------------------
sub spaced    { return () }
sub spaced-ws { return ( ) }

is-deeply spaced(),    (),   '`return ()` returns the empty list';
is-deeply spaced-ws(), (),   '`return ( )` returns the empty list';

# --- a non-empty argument list is unaffected ------------------------------
sub one       { return(5) }
sub two       { return(1, 2) }
sub one-sp    { return 5 }
sub paren-term { return (5) }
sub listy     { return 1, 2 }
sub empty-seq { return Empty }

is-deeply one(),        5,      '`return(5)` returns 5';
is-deeply two(),        (1, 2), '`return(1, 2)` returns the list';
is-deeply one-sp(),     5,      '`return 5` returns 5';
is-deeply paren-term(), 5,      '`return (5)` returns 5';
is-deeply listy(),      (1, 2), '`return 1, 2` returns the list';
is-deeply empty-seq(),  Empty,  '`return Empty` returns Empty';

# --- the same in a method and in an anonymous sub -------------------------
class WithMethod { method m { return() } }
is-deeply WithMethod.m, Nil, '`return()` in a method returns Nil';

my $anon = sub { return(); 1 };
is-deeply $anon(), Nil, '`return()` in an anonymous sub returns Nil';

# vim: expandtab shiftwidth=4
