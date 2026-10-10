use Test;

# Source: Display::Listings (zef ecosystem) passes `-> Str:D $p, Regex:D $pat, ...`
# to a `&c:(Str:D $p, Regex $pat, ...)` parameter. Rakudo checks the expected
# parameter types against the candidate's, with definiteness smileys narrowing.

plan 12;

sub bare(&c:(Int $x)) { 'bound' }
sub defin(&c:(Int:D $x)) { 'bound' }
sub any-t(&c:(Any $x)) { 'bound' }

lives-ok { bare(-> Int:D $y { 1 }) }, 'Int:D candidate binds to expected Int';
lives-ok { bare(-> Int:U $y { 1 }) }, 'Int:U candidate binds to expected Int';
lives-ok { bare(-> Int $y { 1 }) },   'Int candidate binds to expected Int';
dies-ok  { bare(-> Any:D $y { 1 }) }, 'Any:D candidate does not bind to expected Int';
dies-ok  { bare(-> Str $y { 1 }) },   'unrelated type does not bind';

lives-ok { defin(-> Int:D $y { 1 }) }, 'Int:D binds to expected Int:D';
dies-ok  { defin(-> Int:U $y { 1 }) }, 'Int:U does not bind to expected Int:D';
dies-ok  { defin(-> Int $y { 1 }) },   'bare Int does not bind to expected Int:D';

lives-ok { any-t(-> Int $y { 1 }) }, 'narrower candidate binds to expected Any';
dies-ok  { any-t(-> Mu $y { 1 }) },  'Mu candidate does not bind to expected Any';

sub take(&c:(Str:D $p, Regex $pat, Str:D @f, %r)) { 'ok' }
my &f = -> Str:D $prefix, Regex:D $pattern, Str:D @fields, %r { 1 };
is take(&f), 'ok', 'Display::Listings include-row shape binds';
is-deeply (:(Int:D $a) ~~ :(Int $b)), True, 'Signature smartmatch agrees';

done-testing;
