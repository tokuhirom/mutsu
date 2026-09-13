use v6;
use Test;

# #8363: a package-qualified callsite `Pkg::sub()` for a single-letter
# package spelled `S`/`s`/`m`/`ss` (colliding with a quote-like operator
# sigil) used to misparse as that operator once its qualified sub was
# called a SECOND time anywhere in the file: `S:` after stripping the
# leading letter starts with `::` (the package separator, not a real
# adverb), so `parse_match_adverbs` consumed nothing and left the bare `:`
# as the candidate opening delimiter -- which mutsu's `is_delim` check
# wrongly accepted. Real raku's grammar flatly rejects a colon as a
# quote-construct delimiter ("Colons may not be used to delimit quoting
# constructs"), so `Pkg::sub()` should never even be considered for it.

plan 10;

package S { our sub foo() { 42 } }
is S::foo(), 42, 'a single qualified call to package S resolves normally';
is S::foo(), 42, 'a second qualified call to the same package does not misparse';
is S::foo(), 42, 'a third call still resolves normally';

package M { our sub bar() { 99 } }
is M::bar(), 99, 'package M (colliding with m//) resolves';
is M::bar(), 99, 'and again on a second call';

package Q { our sub baz() { 7 } }
is Q::baz(), 7, 'package Q (colliding with the Q string quote) resolves';
is Q::baz(), 7, 'and again on a second call';

# The quote-like operators (other than `S`, which `package S` above
# legitimately shadows for the rest of this compilation unit -- exactly as
# rakudo shadows it too) must still work, including their colon-adverb
# forms.
my $a = "aXaXa";
$a ~~ s:g/a/Y/;
is $a, 'YXYXY', 's:g/.../.../ (colon adverb before the delimiter) still works';

my $b = "AXA";
ok $b ~~ m:i/ax/, 'm:i/.../ (colon adverb before the delimiter) still works';

my $c = "abc";
$c ~~ ss/b/-/;
is $c, 'a-c', 'ss/.../.../ still works';
