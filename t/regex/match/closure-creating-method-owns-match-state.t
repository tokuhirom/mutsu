use Test;

# A method that creates closures owns its `$/`, `$_` and `$!` like any other
# routine (#12882, ADR-12529): its frame root stops the lookup, so it never
# reads its caller's match, topic or error. Every expectation was checked
# against rakudo.

plan 7;

'ab' ~~ /a/;

class D {
    method plain     { $/.raku }
    method with-map  { my @a = (1, 2).map({ $_ }); $/.raku }
    method topic     { my @a = (1, 2).map({ $_ }); $_.raku }
    method error     { my @a = (1, 2).map({ $_ }); $!.raku }
    method captures  { 'xy' ~~ /x/; my $c = { ~$/ }; $c }
    method role-pun  { my $x = 'outer'; -> { $x } }
}

is D.new.plain, 'Nil', 'a method without closures does not see the caller\'s $/';
is D.new.with-map, 'Nil', 'a method that creates closures does not see the caller\'s $/';

$_ = 'caller topic';
is D.new.topic, 'Any', 'a method that creates closures does not see the caller\'s $_';

try die 'caller error';
is D.new.error, 'Nil', 'a method that creates closures does not see the caller\'s $!';

is D.new.captures.(), 'x', 'a closure made in a method captures the method\'s own $/';
is D.new.role-pun.(), 'outer', 'a closure made in a method captures the method\'s lexical';

role R[$t] {
    my $plain = 'plain';
    method mk() { -> { "$plain {$t.name}" } }
}
sub g() { }
&g does R[&g];
is &g.mk().(), 'plain g', 'a closure made in a punned role method captures the role body lexical';
