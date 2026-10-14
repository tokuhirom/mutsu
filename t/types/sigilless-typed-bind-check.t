use v6;
use Test;

# #12907 (PDF `t/write-pdf.t`, `my UInt \startxref = $.offset`): a typed
# sigilless declaration binds its initializer, so the constraint is checked
# against the value a variable holds (not the container wrapper it is passed
# in), and a mismatch is X::TypeCheck::Binding, whether it is spelled `=` or
# `:=`. Every expectation is Rakudo v2026.09's output.

plan 12;

my $o = 263;
my $neg = -1;
subset Pos of Int where * > 0;

{ my UInt \y = $o; is y, 263, 'UInt from a variable'; }
{ my Pos \p = $o; is p, 263, 'a user subset from a variable'; }
{ my Int \i = $o; is i, 263, 'a plain type from a variable'; }
{ my UInt \y := $o; is y, 263, '`:=` spelling'; }
{ my UInt \y = $o; is y + 1, 264, 'the name reads the value'; }

class C {
    has $.offset = 5;
    method m { my UInt \s = $.offset; s }
}
is C.new.m, 5, 'from an attribute accessor';

{ my @a = 1, 2; my Int \e = @a[1]; is e, 2, 'from an array element'; }
{ my %h = a => 3; my Pos \hv = %h<a>; is hv, 3, 'from a hash element'; }

throws-like { my UInt \z = $neg; z }, X::TypeCheck::Binding,
    'a UInt mismatch is a binding failure';
throws-like { my Pos \z = $neg; z }, X::TypeCheck::Binding,
    'a subset mismatch is a binding failure';
throws-like { my Int \z = "x"; z }, X::TypeCheck::Binding,
    'a plain type mismatch is a binding failure',
    message => 'Type check failed in binding; expected Int but got Str ("x")';
{ my UInt \q = 5; is q, 5, 'from a literal'; }
