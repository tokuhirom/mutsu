use Test;

plan 8;

sub msg(&c) { my $m; try { c(); CATCH { default { $m = .^name ~ ': ' ~ .message } } }; $m }

# A destructive s/// on an immutable topic is an X::AdHoc in rakudo, worded by
# what the topic is bound to.
sub with-param($_) { s/a/b/ }
is msg({ with-param('a') }), 'X::AdHoc: Cannot assign to a readonly variable or a value',
    'a readonly $_ parameter';
is msg({ for 'a' { s/a/b/ } }), 'X::AdHoc: Cannot assign to an immutable value',
    'for over a literal';
is msg({ for <a b> { s/a/b/ } }), 'X::AdHoc: Cannot assign to an immutable value',
    'for over a quote-words list';
is msg({ for 1, 2 { s/1/3/ } }), 'X::AdHoc: Cannot assign to an immutable value',
    'for over Int literals';
is msg({ given 'abc' { s/a/b/ } }), 'X::AdHoc: Cannot assign to an immutable value',
    'given a literal';
is msg({ with 'a' { s/a/b/ } }), 'X::AdHoc: Cannot assign to an immutable value',
    'with a literal';
is msg({ my @x = <hello world>.map: { s/h/H/ } }), 'X::AdHoc: Cannot assign to an immutable value',
    'map over a quote-words list (Language/faq)';
# a non-matching s/// stays a no-op
is msg({ for 'a' { s/z/b/ } }), Any, 'no substitution, no error';
