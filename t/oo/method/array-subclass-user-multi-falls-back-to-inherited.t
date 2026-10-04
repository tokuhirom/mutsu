use Test;

# Intl::CLDR: a user `multi method AT-POS` adds candidates to the inherited one.
plan 3;

class MW is Array {
    multi method AT-POS($, :$leap!) { self.Array::AT-POS(13) }
}
my $m = MW.new;
$m[0] = 'a';
$m[13] = 'leap';
is $m[0], 'a', 'a plain index reaches Array.AT-POS';
is $m.AT-POS(13, :leap), 'leap', 'the user candidate still matches';
is $m.AT-POS(0), 'a', 'explicit AT-POS falls back too';
