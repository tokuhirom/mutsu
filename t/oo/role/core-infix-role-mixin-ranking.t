use v6.d;
use Test;

plan 8;

# A user infix candidate is ranked against the core one parameter by
# parameter (rakudo's is_narrower), for an operand with a role mixed in.
role U[\d, \u] { }

sub cell(Str $a, Str $b) {
    my $r = EVAL qq[
        multi sub infix:<*>($a \$v, $b \$n) \{ "user" \}
        my \$q = 1.5 but U["l", "pt"];
        (try \$q * 2) // \$!.message.lines[0].substr(0, 9);
    ];
    $r;
}

is cell('Rat', 'Int'), 'user', 'narrower in one parameter, same in the other: user wins';
is cell('Rat', 'Real'), '3', 'narrower in one, wider in the other: core wins';
is cell('Rational', 'Rational'), '3', 'wider in the second parameter: core wins';
is cell('Rational', 'Int'), 'Ambiguous', 'same types: ambiguous';
is cell('U', 'Int'), 'Ambiguous', 'an unrelated role type with the same second type: ambiguous';
is cell('U', 'Rational'), '3', 'unrelated role, wider second parameter: core wins';
is cell('Any', 'Int'), '3', 'wider first parameter: core wins';
is cell('Mu', 'Numeric'), '3', 'wider in both: core wins';
