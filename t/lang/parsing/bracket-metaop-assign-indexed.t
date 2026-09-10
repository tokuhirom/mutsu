use v6;
use Test;

# A bracket meta-op compound assignment (`[op]=`) must work on an indexed /
# subscripted lvalue, not just a bare variable: `%h<k> [R//]= $v`, `@a[$i] [+]= 1`.
# Previously the precedence loop grabbed `[op]` as a bracket *infix* operator and
# choked on the trailing `=` ("expression after bracket ... operator"). Regression
# surfaced by PostCocoon::Url (`$result<port> [R//]= ~.<port>`).

plan 10;

# reduce meta (`[+]=` behaves like `+=`) on a hash angle-subscript
my %h;
%h<n> = 5;
%h<n> [+]= 10;
is %h<n>, 15, "[+]= on hash angle-subscript";

# `[*]=` on an array positional-subscript
my @a = 2, 3, 4;
@a[1] [*]= 10;
is @a[1], 30, "[*]= on array subscript";

# reversed defined-or meta `[R//]=` : `\$x = \$rhs // \$x` -> assigns to the LHS
# only when the RHS is defined (R reverses the operands of //)
my %r;
%r<x> [R//]= "y";
is %r<x>, "y", "[R//]= assigns when LHS undefined";

my %r2;
%r2<x> = "keep";
%r2<x> [R//]= "new";
is %r2<x>, "new", "[R//]= with R reversal takes the right operand when defined";

# `[//]=` (plain defined-or) leaves a defined LHS untouched
my %d;
%d<k> = "orig";
%d<k> [//]= "fallback";
is %d<k>, "orig", "[//]= keeps a defined value";

my %d2;
%d2<k> [//]= "fallback";
is %d2<k>, "fallback", "[//]= fills an undefined value";

# named reduction op `[max]=` on a subscript
my @m = 3, 1;
@m[0] [max]= 10;
is @m[0], 10, "[max]= on array subscript";
@m[0] [max]= 2;
is @m[0], 10, "[max]= keeps the larger value";

# bare-variable form still works (unchanged path)
my $s = 1;
$s [+]= 4;
is $s, 5, "[+]= on a bare scalar still works";

# prefix reduction `[+] @list` is unaffected by the guard
is ([+] 1, 2, 3, 4), 10, "prefix reduction [+] still parses";
