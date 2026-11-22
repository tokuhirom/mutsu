use Test;

# A regex value matched again and again (a loop over `~~`) has the facts a
# match derives from its source tree decided once and kept with the tree
# (#18003). Everything that can change between two matches of the same value
# must still be seen: an interpolated variable's new value, the type of value
# it holds, the package the match runs in. Expected values are rakudo's.

plan 8;

my @names = <libfoo.so.1 libreadline.so.8 LIBREADLINE.SO.7 libreadline.so libbar.so.2>;
my @hits = @names.grep({ $_ ~~ rx/:i libreadline\.so\.(\d+) $/ }).map({ ~$/[0] });
is @hits.join(','), '8,7', 'one regex value matched in a loop keeps its captures straight';

my $word = 'foo';
my $rx = rx/ ^ $word \d /;
my @seen;
for <foo1 bar2 foo3> -> $s {
    @seen.push: so $s ~~ $rx;
    $word = 'bar' if $s eq 'foo1';
}
is @seen.join(','), 'True,True,False', 'an interpolated scalar is read at each match';

my $alt = 'x';
my $rx2 = rx/ <$alt> /;
my @kinds;
for 1..3 -> $i {
    @kinds.push: so 'abc' ~~ $rx2;
    $alt = $i == 1 ?? rx/b/ !! 'z';
}
is @kinds.join(','), 'False,True,False', 'an interpolated value may change type between matches';

my @arr = <q r>;
my $rx3 = rx/ @arr /;
my @arr-hits;
for 1..2 {
    @arr-hits.push: so 'abc' ~~ $rx3;
    @arr = <b>;
}
is @arr-hits.join(','), 'False,True', 'an interpolated array is read at each match';

package P1 { our sub try($rx, $s) { so $s ~~ $rx } }
package P2 { our sub try($rx, $s) { so $s ~~ $rx } }
my $shared = rx/ ab+c /;
is (P1::try($shared, 'xabbc'), P2::try($shared, 'xac'), P1::try($shared, 'abc')).join(','),
  'True,False,True', 'one regex value matched from two packages';

my token digit-pair { \d \d }
my $sub = rx/ ^ <digit-pair> $ /;
is (('12' ~~ $sub).Bool, ('1' ~~ $sub).Bool).join(','), 'True,False',
  'a subrule call through a repeated regex value';

my $count = 0;
for ^50 { $count++ if "item$_" ~~ / item \d ** 2 / }
is $count, 40, 'a literal regex in a loop body';

my $ci = rx:i/ ABC /;
is (('xabcx' ~~ $ci).Str, ('AbC' ~~ $ci).Str).join(','), 'abc,AbC',
  'an adverbed regex value matched twice';
