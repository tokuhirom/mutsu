use Test;

plan 6;

# `<&name>` calls the lexical `&name`. When that lexical holds the `&re`
# reference to a `my regex`/`token` (rather than an anonymous regex literal),
# the subrule must still call the referenced declaration.
# From URI::Query::FromHash, whose `escape` takes `&class = &should-escape`
# and matches with `/<&class>/`.

my regex amp { '&' }
my token digit-run { \d+ }

my &c = &amp;
is ~("a&b" ~~ /<&c>/), '&', '<&c> where my &c = &named-regex';

sub via-param(&class) { ~("a&b" ~~ /<&class>/) }
is via-param(&amp), '&', '<&class> where &class is a parameter';

sub via-default($s, &class = &amp) { $s.subst(:g, /<&class>/, 'X') }
is via-default('a & b & c'), 'a X b X c', '<&class> with a defaulted &class parameter in subst';

sub with-token(&t) { ~("ab123cd" ~~ /<&t>/) }
is with-token(&digit-run), '123', 'a my token passed through a &param';

my &anon = /'&'/;
is ~("x&y" ~~ /<&anon>/), '&', 'an anonymous regex in &var still works';

is ~("a&b" ~~ /<&amp>/), '&', 'the named regex itself still resolves';
