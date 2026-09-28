use v6.d;

# Fixture for t/oo/attribute/attribute-where-module-private-sub.t: an
# attribute `where` block that calls a sub private to this compunit, the shape
# of the ecosystem Date::Calendar::Gregorian `$.locale` attribute.
unit class AttrWhereUnitSub;

has Str $.locale is rw where { check-locale($_) } = 'en';

method BUILD(:$locale) {
    $!locale = $locale // 'en';
}

method set-locale($locale) { $!locale = $locale }

sub check-locale($locale) {
    so $locale eq any(<en it fr>);
}
