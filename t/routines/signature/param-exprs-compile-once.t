use Test;

# A parameter's `where` clause, default and shape dimension are compiled once,
# with the routine, and run on every call (ADR-0133, #10107). These pin the
# lexical view the precompiled chunks must keep: routine and outer lexicals,
# earlier parameters, `$_`/WhateverCode forms, placeholders, the declaring
# package, and a caller lexical the clause writes.

plan 33;

# --- where: the three clause shapes, called repeatedly -----------------------
sub w-whatever(Int $x where * > 0) { $x }
sub w-block(Int $x where { $_ > 0 }) { $x }
sub w-method($x where .so) { $x }
for ^3 {
    is w-whatever(5), 5, "WhateverCode where accepts (call $_)";
}
dies-ok { w-whatever(0) }, 'WhateverCode where rejects';
is w-block(2), 2, 'block where accepts';
dies-ok { w-block(-1) }, 'block where rejects';
is w-method(1), 1, '.method where accepts';
dies-ok { w-method(0) }, '.method where rejects';

# --- where: placeholder, earlier parameter, sigilless earlier parameter ------
sub w-placeholder($x where { $^n %% 2 }) { $x }
is w-placeholder(4), 4, 'placeholder where accepts';
dies-ok { w-placeholder(3) }, 'placeholder where rejects';

sub w-earlier($lo, $x where * > $lo) { $x }
is w-earlier(1, 2), 2, 'where reads an earlier parameter';
dies-ok { w-earlier(5, 2) }, 'where on an earlier parameter rejects';

sub w-sigilless(\lo, $x where * > lo) { $x }
is w-sigilless(1, 2), 2, 'where reads an earlier sigilless parameter';
dies-ok { w-sigilless(5, 2) }, 'sigilless-parameter where rejects';

# --- where: outer and per-closure lexicals -----------------------------------
my $limit = 10;
sub w-outer($x where * < $limit) { $x }
is w-outer(3), 3, 'where reads an outer lexical';
$limit = 2;
dies-ok { w-outer(3) }, 'where sees the outer lexical updated after declaration';

my @checkers = (1, 5).map(-> $lim { -> $x where * <= $lim { $x } });
is @checkers[0](1), 1, 'closure where reads its own captured lexical';
dies-ok { @checkers[0](2) }, 'closure where rejects past its own captured lexical';
is @checkers[1](5), 5, 'a second closure from the same literal has its own capture';

# --- where: a clause that throws, and one that writes a caller lexical -------
sub w-throws($x where { $_ > 0 or die "custom: $_" }) { $x }
throws-like { w-throws(-3) }, Exception, message => /'custom: -3'/,
    'a throwing where clause propagates its own exception';

my $trace = '';
sub w-writes($x where { $trace ~= 'w'; True }) { $x }
w-writes(1) for ^3;
is $trace, 'www', 'a where clause writing a caller lexical runs once per call';

# --- multi: where is a discriminator ------------------------------------------
subset Small of Int where * < 100;
multi size(Small $x)                 { 'small' }
multi size(Int $x where * < 10_000)  { 'medium' }
multi size(Int $x)                   { 'large' }
is (size(5), size(5000), size(50_000)).join(','), 'small,medium,large',
    'multi candidates select on subset and where';
is (^3).map({ size(5000) }).join(','), 'medium,medium,medium',
    'repeated dispatch keeps selecting the where candidate';

# --- defaults -----------------------------------------------------------------
my $base = 40;
sub d-outer($x = $base + 2) { $x }
is d-outer(), 42, 'non-literal default reads an outer lexical';
$base = 1;
is d-outer(), 3, 'default re-evaluated per call';
sub d-earlier($a, $b = $a * 2) { $b }
is d-earlier(21), 42, 'default reads an earlier parameter';
sub d-fresh(@a = []) { @a.push(1); @a.elems }
is (d-fresh(), d-fresh()).join(','), '1,1', 'a container default is fresh per call';

# --- the declaring package ----------------------------------------------------
package P {
    our $default = 'from-P';
    our sub d-pkg($x = $?PACKAGE.^name) { $x }
    our sub d-our($x = $default) { $x }
}
is P::d-pkg(), 'P', '$?PACKAGE in a default is the declaring package';
is P::d-our(), 'from-P', 'a package variable in a default resolves in its package';

# --- methods ------------------------------------------------------------------
class C {
    has $.min = 3;
    method m($x where * >= $!min) { $x }
    method d($x = $!min * 2) { $x }
}
is C.new.m(4), 4, 'method where reads an attribute';
dies-ok { C.new(:min(10)).m(4) }, 'method where rejects per instance';
is C.new(:min(5)).d(), 10, 'method default reads an attribute';

# --- shape dimensions ---------------------------------------------------------
my $n = 2;
sub s-shape(@a[$n]) { @a.elems }
my @two[2] = 1, 2;
is s-shape(@two), 2, 'shape dimension reads an outer lexical';

done-testing;
