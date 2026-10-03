use Test;

# Longest-token rule at the power level: a user infix whose symbol starts with
# `**` wins over the built-in `**` (#11323).

plan 6;

my sub infix:<**+>($a, $b) is equiv(&[~]) { "$a!$b" }
is 2 **+ 3, '2!3', 'a looser user infix starting with ** is one token';
is 1 + 2 **+ 3, '3!3', 'it parses at its own (concatenation) level';
is 2 ** +3, 8, 'the built-in ** with a prefix + on the right still works';
is 2 ** 3, 8, 'a plain ** is unchanged';

my sub infix:<**%>($a, $b) is equiv(&[**]) { "$a%$b" }
is 2 **% 3, '2%3', 'a user infix declared at the power level';

my sub infix:<***>($a, $b) { "$a*$b" }
is 2 *** 3 ~ 1, '2*31', 'a trait-less user infix starting with **';
