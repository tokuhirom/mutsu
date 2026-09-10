use Test;

plan 7;

# Three parser bugs that block popular ecosystem modules (URI, HTTP::Tiny,
# YAMLish), each reduced to a minimal construct.

# 1. Typed invocant followed directly by a return constraint (URI).
class IB { method m(IB:D: --> Str) { "ok" } }
is IB.new.m, "ok", 'typed invocant + return constraint in method signature';

class IB2 { multi method n(IB2:D: --> Int) { 7 } }
is IB2.new.n, 7, 'multi method typed invocant + return constraint';

# 2. `.=` update assignment whose RHS is a sub reference (`$x .= &f`) — HTTP::Tiny.
sub dbl($x) { $x * 2 }
my $v = 21;
$v .= &dbl;
is $v, 42, '.= with a &sub reference (method-call-via-sub update)';

my $w = 10;
sub addn($x, $n) { $x + $n }
$w .= &addn(5);
is $w, 15, '.= &sub(args) passes extra arguments';

# 3. Grammar named-variant declarator `:<name>` (YAMLish).
grammar G {
    rule TOP { <element> }
    proto token element {*}
    token element:<null> { 'null' }
    token element:<true> { 'true' }
}
ok G.^name eq 'G', 'grammar with token NAME:<variant> declarators parses';

# The `:sym<...>` long form still works.
grammar G2 {
    proto token op {*}
    token op:sym<plus> { '+' }
}
ok G2.^name eq 'G2', 'token NAME:sym<variant> still parses';

# Plain token still works.
grammar G3 { token word { \w+ } }
ok G3.parse('hello', :rule<word>), 'plain token still parses and matches';
