use v6;
use Test;

plan 41;

# ---------------------------------------------------------------------------
# A parenthesized TERM containing top-level `;` is a statement list: one
# element per statement, and the final `;` is a terminator (it does NOT add a
# trailing empty element). A single statement is NOT wrapped in a list.
# ---------------------------------------------------------------------------

my $one-trailing = ('foo';);
is $one-trailing, 'foo', "('foo';) is the bare value, not a 1-element list";
ok $one-trailing ~~ Str, "('foo';) stays a Str";

my $plain = ('foo');
is $plain, 'foo', "('foo') is a plain parenthesized expression";
ok $plain ~~ Str, "('foo') stays a Str";

my $two = ('foo'; 'bar');
ok $two ~~ List, "('foo'; 'bar') is a List";
is $two.elems, 2, "('foo'; 'bar') has 2 elements";
is $two[0], 'foo', "('foo'; 'bar')[0]";
is $two[1], 'bar', "('foo'; 'bar')[1]";

my $two-trailing = ('foo'; 'bar';);
ok $two-trailing ~~ List, "('foo'; 'bar';) is a List";
is $two-trailing.elems, 2, "trailing ; adds no element to a paren term";

my $empty = ();
ok $empty ~~ List, "() is a List";
is $empty.elems, 0, "() is empty";

my $three = (1; 2; 3);
is $three.elems, 3, "(1; 2; 3) has 3 elements";
is $three[0] + $three[1] + $three[2], 6, "(1; 2; 3) element values";

# A statement whose own value is a list becomes ONE nested element.
my $nested = (1, 2; 3);
is $nested.elems, 2, "(1,2; 3) has 2 elements";
is $nested[0].elems, 2, "(1,2; 3)[0] is the sub-list (1,2)";
is $nested[1], 3, "(1,2; 3)[1] is 3";

my $nested-trailing = (1, 2; 3;);
is $nested-trailing.elems, 2, "(1,2; 3;) trailing ; adds no element";

# ---------------------------------------------------------------------------
# A `;` inside a CALL ARGUMENT LIST is a different construct: it separates
# whole argument lists (rakudo's semiarglist). Each `;`-separated section
# becomes one List-valued argument, and because an argument list may be empty a
# TRAILING `;` DOES contribute a final empty-list argument.
# ---------------------------------------------------------------------------

sub count-args(|c) { c.elems }
sub nth-arg(|c) { c }

is count-args('foo'), 1, "f('foo') passes 1 argument";
is count-args(), 0, "f() passes no arguments";
is count-args('foo'; 'bar'), 2, "f(a; b) passes 2 arguments";
is count-args('foo';), 2, "f(a;) passes 2 arguments (trailing empty slice)";
is count-args('foo'; 'bar';), 3, "f(a; b;) passes 3 arguments";
is count-args(;), 2, "f(;) passes 2 empty arguments";
is count-args(;;), 3, "f(;;) passes 3 empty arguments";
is count-args(1, 2; 3), 2, "f(1,2; 3) passes 2 arguments";

my $c = nth-arg('foo';);
is $c[0].elems, 1, "f(a;) first argument is the 1-element list (a,)";
is $c[1].elems, 0, "f(a;) second argument is the empty list";

# ---------------------------------------------------------------------------
# `;` inside a SUBSCRIPT is a multidimensional index -- NOT a statement list.
# These must keep working exactly as before.
# ---------------------------------------------------------------------------

my @m[2;3];
@m[0;0] = 'a';
@m[1;2] = 'b';
is @m[0;0], 'a', '@m[0;0] multidim subscript';
is @m[1;2], 'b', '@m[1;2] multidim subscript';

my @flat = 10, 11, 12, 13;
is @flat[1, 2].join(','), '11,12', '@a[1,2] comma slice is unchanged';

# ---------------------------------------------------------------------------
# `enum`'s parenthesized body is that same parenthesized term, so `;` works as
# a variant separator alongside `,`, with or without a trailing separator.
# ---------------------------------------------------------------------------

enum SemiEnum (SE_A => 0; SE_B => 10);
is SemiEnum.enums.elems, 2, 'enum with ; separated variants';
is SemiEnum.enums<SE_A>, 0, 'enum ; variant value 1';
is SemiEnum.enums<SE_B>, 10, 'enum ; variant value 2';

enum SemiTrailEnum (ST_A => 1; ST_B => 2;);
is SemiTrailEnum.enums.elems, 2, 'enum with trailing ; declares no extra variant';

enum CommaEnum (CE_A => 1, CE_B => 2);
is CommaEnum.enums.elems, 2, 'enum with , separated variants still works';

enum CommaTrailEnum (CT_A => 1,);
is CommaTrailEnum.enums.elems, 1, 'enum with a single trailing-comma variant';

enum SingleSemiEnum (SS_A => 7;);
is SingleSemiEnum.enums<SS_A>, 7, 'enum with a single ; terminated variant';

# The multi-line spelling from Language/nativecall.rakudoc.
enum AddrInfo-Family (
    AF_UNSPEC => 0;
    AF_INET   => 2;
    AF_INET6  => 10;
);
is AddrInfo-Family.enums.elems, 3, 'multi-line ; separated enum body';
is AddrInfo-Family.enums<AF_INET6>, 10, 'multi-line ; separated enum value';

# A computed body still works (it is one expression, not a static variant list).
enum ComputedEnum (1..3 Z=> <x y z>);
is ComputedEnum.enums.elems, 3, 'computed enum body still works';

done-testing;
