use Test;

# A variable passed as an argument is dispatched and bound by its own
# declared type, which the call site reads in the variable's own frame.
# A same-named typed variable of a routine further up the call chain
# never stands in for it (ADR-13757). Checked against rakudo.

plan 8;

multi m(int $x) { 'native' }
multi m($x) { 'plain' }

sub untyped-callee($got) { m($got) }
sub native-caller(int $got) { untyped-callee($got) }
is native-caller(1), 'plain', 'an untyped parameter ignores the caller\'s int $got';

sub untyped-local() { my $p = 1; m($p) }
sub native-local-caller() { my int $p = 5; untyped-local() }
is native-local-caller(), 'plain', 'an untyped local ignores the caller\'s my int $p';

my int $n = 3;
is m($n), 'native', 'a program-scope native variable keeps its type';

my int $q = 4;
sub reads-program() { m($q) }
sub shadowing-caller() { my $q = 1; reads-program() }
is shadowing-caller(), 'native', 'a routine reads the program variable\'s type, not its caller\'s';

sub alias(\x) { x = 's'; x }
sub copies($v is copy) { alias($v) }
sub typed-top() { my Int $v = 1; copies(1) }
is typed-top(), 's', 'a sigilless parameter checks the bound variable\'s own type';

sub rw-target($x is rw) { $x = 's'; $x }
sub plain-local() { my $w = 1; rw-target($w) }
sub typed-local-top() { my Int $w = 1; plain-local() }
is typed-local-top(), 's', 'an is rw parameter checks the bound variable\'s own type';

my Int $t = 1;
sub rw-typed() { rw-target($t) }
sub plain-shadow() { my $t; rw-typed() }
throws-like { plain-shadow() }, X::TypeCheck,
    'an is rw store into a typed program variable still checks its type';

sub typed-alias() { my Int $a = 1; alias($a) }
throws-like { typed-alias() }, X::TypeCheck,
    'a sigilless store into a typed local still checks its type';
