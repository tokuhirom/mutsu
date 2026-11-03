use Test;

plan 6;

# While no class declares `is built(:bind)`, a named argument written as
# `key => $var` need not retain $var's container; semantics must be unchanged.
class Plain { has $.x; has $.y }

my $v = 3;
my $p = Plain.new(x => $v, y => $v * 2);
$v = 10;
is $p.x, 3, 'a constructed attribute holds the value, not the caller variable';
is $p.y, 6, 'an expression argument is a plain value';

sub grab(*%h) { %h<a> = 99; %h<a> }
my $a = 1;
is grab(a => $a), 99, 'the callee may rewrite its named hash entry';

sub named(:$k) { $k }
my $kv = 7;
is named(k => $kv), 7, 'a named parameter reads the value';

# A bind class declared later (here through EVAL) still binds the caller's
# container for the calls made after its declaration.
EVAL 'class Late { has $.v is built(:bind) }';
my $src = 3;
my $late = ::('Late').new(v => $src);
$src = 4;
is $late.v, 4, 'a bind attribute declared at run time binds the caller Scalar';
is Plain.new(x => $src).x, 4, 'a plain class built afterwards still sees the value';
