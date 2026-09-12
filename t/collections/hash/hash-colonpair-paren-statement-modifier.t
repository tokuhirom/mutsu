use v6;
use Test;

plan 7;

# A colonpair's parenthesized value takes a statement modifier, exactly as a
# plain parenthesized group does: `:title( S/(' ')$// given @e[0] ~ @e[1] )` is
# the shape Data::Dump::Tree's `Horizontal` uses. Without it the modifier
# keyword sat where the closing `)` was expected and the whole argument list
# failed to parse.
# Every assertion below was checked against rakudo itself.

sub take-named(:$t) { $t }

{
    my $x = 'a';
    is take-named(:t( $x.uc given $x )), 'A', '`given` inside a colonpair value';
    is take-named(:t( $x.uc if 1 )), 'A', '`if` inside a colonpair value';
    is take-named(:t( $x.uc if 0 )).raku, 'Empty',
        'an unrun `if` yields Empty, as a statement modifier does';
}

# Statement modifiers are looser than the comma, so one after a separated list
# applies to the whole value rather than to its last element.
is take-named(:t(1, 2 given 3)).raku, '$(1, 2)',
    'a modifier after a separated list applies to the whole value';
is take-named(:t(1, 2 if 0)).raku, 'Empty',
    'and can suppress the whole list';

# The spellings with no modifier must be untouched.
is take-named(:t(1, 2)).raku, '$(1, 2)', 'a plain separated list is unchanged';
is take-named(:t(1)).raku, '1', 'a plain single value is unchanged';

done-testing;
