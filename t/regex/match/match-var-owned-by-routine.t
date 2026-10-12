use Test;

# Every routine has its own `$/`, initially Nil, and its capture views `$0`,
# `$<name>` with it (#12398, ADR-12529 phase 3). A routine must not start with
# its caller's match: neither a plain read nor `m:c` continuing from the
# caller's position may see it.

plan 14;

class Plain { method m { 1 } }

'ab' ~~ /(a)$<n>=(b)/;

sub reads-slash { $/.raku }
is reads-slash(), 'Nil', 'a sub reads its own $/, not the caller one';

sub reads-zero { $0 }
is reads-zero().raku, 'Nil', 'a sub does not see the caller $0';

sub reads-named { $<n> }
is reads-named().raku, 'Nil', 'a sub does not see the caller $<n>';

sub after-method-call { Plain.new.m; $/.raku }
is after-method-call(), 'Nil', 'a method call in the sub does not expose the caller $/';

sub after-map { my @x = (1, 2).map({ $_ * 2 }); Plain.new.m; $/.raku }
is after-map(), 'Nil', 'neither does a closure callback';

class Reader { method rd { $/.raku } }
is Reader.new.rd, 'Nil', 'a method reads its own $/';

my $fmt = '%a %b';
sub continue-from-start { my @s; while $fmt ~~ m:c/ '%' \w / { @s.push(~$/) }; @s }
'xb' ~~ /b/;
is-deeply continue-from-start(), ['%a', '%b'], 'm:c in a sub starts at 0, not at the caller match';

sub recurse($n) { return $/.raku if $n == 0; "x$n" ~~ /\d/; recurse($n - 1) }
is recurse(2), 'Nil', 'a recursive call starts with a fresh $/ as well';

sub owns-after-match { 'hello' ~~ /ell/; ~$/ }
is owns-after-match(), 'ell', 'a sub reads its own match';
is ~$/, 'b', '... which does not reach the caller';

sub given-when { given 'hello' { when /ll/ { ~$/ } } }
is given-when(), 'll', 'given/when matches into the routine $/';

grammar Digits { token TOP { \d+ } }
sub parses { Digits.parse('42'); ~$/ }
is parses(), '42', 'Grammar.parse sets the routine $/';
is ~$/, 'b', '... and leaves the caller $/ alone';

my $s = sub { $/.raku };
is $s(), 'Nil', 'an anonymous sub owns its $/ too';
