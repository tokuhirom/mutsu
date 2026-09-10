use Test;

plan 7;

# An object with both a Numeric and a Str method stringifies via Str, not
# via its numeric value (string interpolation, ~, .Stringy).
class S {
    has $.code;
    has $.title;
    method Int     { $!code }
    method Numeric { $!code }
    method Str     { $!title }
    method gist    { $!title }
}
my $s = S.new(code => 200, title => 'OK');
is ~$s,         'OK', '~obj uses Str, not Numeric';
is "$s",        'OK', 'string interpolation uses Str';
is $s.Stringy,  'OK', '.Stringy delegates to user Str';
is $s.Str,      'OK', '.Str returns the title';
is +$s,         200,  '+obj still uses Numeric';

# A class that does Real with no custom Str still bridges .gist/.Str to the
# number (the numeric bridge is preserved when there is no user Str).
class P does Real {
    has $.n;
    method Bridge { $!n.Num }
}
my $p = P.new(n => 7);
is $p.gist, '7', 'does-Real object gist bridges to number (no user Str)';
ok ($p + 5) == 12, 'does-Real object still bridges arithmetic';
