use v6;
use Test;

# `.=` is a read-modify-write metaop, not the plain assignment its expansion
# spells: raku evaluates the lvalue once as a container and stores into it.
# The distinction only shows through a `$.attr` accessor, where `$.attr` is
# `self.attr` ITEMIZED -- a non-`rw` scalar accessor hands back a bare value, so
# the itemization is a throwaway `Scalar`, the RMW writes into it, the
# expression still yields the computed value, and the attribute is untouched.
# The hand-written `$.attr = $.attr.meth` has no such itemization and dies.
#
# Every row below was measured against raku v2026.07.

plan 25;

# --- `$.attr .= meth` on a non-`rw` scalar accessor: lives, attribute unchanged.

class RoStr { has Str $.s = "a";
    method stmt()  { $.s .= uc; $!s }
    method expr()  { my $r = ($.s .= uc); "$r/$!s" }
    method twice() { $.s .= uc; $.s .= uc; $!s }
    method args()  { $.s .= subst("a", "z"); $!s }
}
is RoStr.new.stmt,  "a",   'statement `$.s .= uc` leaves a non-rw attribute alone';
is RoStr.new.expr,  "A/a", 'expression `$.s .= uc` yields the computed value';
is RoStr.new.twice, "a",   'repeated `$.s .= uc` never accumulates';
is RoStr.new.args,  "a",   '`$.s .= subst(...)` with arguments behaves the same';

class RoInt { has Int $.n = 5;
    method m() { my $r = ($.n .= succ); "$r/$!n" }
}
is RoInt.new.m, "6/5", '`$.n .= succ` on a non-rw Int accessor yields 6, leaves 5';

# --- the plain assignment spelling still dies (it is NOT an RMW).

class RoDies { has Str $.s = "a";
    method m() { $.s = $.s.uc; $!s }
}
throws-like { RoDies.new.m }, X::Assignment::RO,
    '`$.s = $.s.uc` still dies -- the marker must not turn every assignment into an RMW';

# --- `$!attr` (the private slot) is a real container: `.=` writes through.

class Priv { has Str $.s = "a";
    method m() { my $r = ($!s .= uc); "$r/$!s" }
}
is Priv.new.m, "A/A", '`$!s .= uc` writes the attribute slot itself';

# --- an `is rw` accessor hands back the container: `.=` writes through.

class RwStr { has Str $.s is rw = "a";
    method m() { my $r = ($.s .= uc); "$r/$!s" }
}
is RwStr.new.m, "A/A", '`$.s .= uc` on an `is rw` accessor writes the attribute';

# --- `@.a` / `%.h` accessors hand back containers, so they write through even
#     without `is rw` (assigning into a container is a STORE).

class RoList { has @.a = "a", "b"; has %.h = k => "a";
    method arr()  { @.a .= map({ .uc }); @!a.join(",") }
    method hash() { %.h<k> .= uc; %!h<k> }
}
is RoList.new.arr,  "A,B", '`@.a .= map` writes a non-rw array attribute';
is RoList.new.hash, "A",   '`%.h<k> .= uc` writes a non-rw hash attribute';

# --- plain lexicals are unaffected by the marker.

{
    my $s = "a";
    my $r = ($s .= uc);
    is "$r/$s", "A/A", '`my $x .= uc` still writes the lexical';
}
{
    my $s = "ab";
    $s .= uc .= flip;
    is $s, "BA", 'a chained `.=` keeps mutating the same lvalue';
}
{
    my $s = "a-b";
    $s .= subst("-", "+");
    is $s, "a+b", '`.=` with parenthesized arguments';
}
{
    my $s = "a-b";
    $s .= subst: "-", "+";
    is $s, "a+b", '`.=` with colon arguments';
}
{
    my $s = "a";
    $s .= "uc"();
    is $s, "A", '`.=` with a quoted method name';
}
{
    my @a = "a", "b";
    @a[0] .= uc;
    is @a.join(","), "A,b", '`.=` on an array element';
}
{
    my %h = k => "a";
    %h<k> .= uc;
    is %h<k>, "A", '`.=` on a hash element';
}
{
    my @a = "a", "b";
    @a .= map({ .uc });
    is @a.join(","), "A,B", '`.=` on a whole array';
}
{
    my @a = "a", "b";
    @a>>.=uc;
    is @a.join(","), "A,B", 'hyper `>>.=`';
}
{
    my Int $x .= new;
    is $x, 0, '`my Int $x .= new` declaration form';
}
{
    my $s = "a";
    my $t = ($s .= uc) ~ "!";
    is "$t/$s", "A!/A", '`.=` nested inside a larger expression';
}
{
    my $s = "a";
    is join("|", $s .= uc, $s), "A|A", '`.=` as a call argument';
}
{
    my $s = "a";
    $s .= uc if 1;
    is $s, "A", '`.=` with a statement modifier';
}

# --- `.=` binds at method-postfix (dotty infix) precedence, so it is the one
#     assignment-like expression legal unparenthesized inside `?? !!`, while the
#     loose `=` / `OP=` forms are rejected. This used to be decided by scanning
#     the branch's source text; the `.=` marker answers it from the AST now.
{
    my $s = "a";
    1 ?? $s .= uc !! "x";
    is $s, "A", 'unparenthesized `.=` is legal inside a ternary branch';
}
throws-like 'my $a = 5; $a ?? $a += 1 !! $a', X::Syntax::ConditionalOperator::PrecedenceTooLoose,
    'a LOOSE compound assignment in a ternary branch is still rejected';

done-testing;
