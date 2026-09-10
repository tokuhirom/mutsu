use Test;

# Hyper function-op (`>>[&func]<<`) with a mutating, sigilless `rw` code-ref
# must write the mutated value back into the left lvalue, both for named subs
# and for code-refs held in lexical `&` variables (as S03-metaops/infix.t does).

plan 19;

sub cst (\a, \b) { "foo" }
sub csta(\a, \b) { a = "foo" }

# --- scalar, named code-ref ---
{
    my $a = 1;
    my $r = $a >>[&cst]<< 6;
    is $r, "foo", "scalar >>[&cst]<< returns result";
    is $a, 1, "non-mutating op leaves lvalue unchanged";
}
{
    my $a = 1;
    my $r = $a >>[&csta]<< 6;
    is $r, "foo", "scalar >>[&csta]<< returns result";
    is $a, "foo", "mutating op writes back to lvalue";
}

# --- scalar, lexical code-ref variable ---
{
    my &op = &cst;
    my &metaop = &csta;
    my $a = 1;
    is ($a >>[&op]<< 6), "foo", "lexical &op returns result";
    is $a, 1, "lexical non-mutating op leaves lvalue unchanged";
    my $b = 1;
    is ($b >>[&metaop]<< 6), "foo", "lexical &metaop returns result";
    is $b, "foo", "lexical mutating op writes back";
}

# --- array ---
{
    my @a = 1, 2, 3;
    my @r = @a >>[&csta]<< (6, 7, 8);
    is-deeply @r, ["foo", "foo", "foo"], "array >>[&csta]<< returns results";
    is-deeply @a, ["foo", "foo", "foo"], "array mutating op writes back";
}
{
    my @a = 1, 2, 3;
    my @r = @a >>[&cst]<< (6, 7, 8);
    is-deeply @r, ["foo", "foo", "foo"], "array >>[&cst]<< returns results";
    is-deeply @a, [1, 2, 3], "array non-mutating op leaves lvalue unchanged";
}

# --- array, lexical code-ref ---
{
    my &metaop = &csta;
    my @a = 1, 2, 3;
    my @r = @a >>[&metaop]<< (6, 7, 8);
    is-deeply @r, ["foo", "foo", "foo"], "array lexical &metaop returns results";
    is-deeply @a, ["foo", "foo", "foo"], "array lexical mutating op writes back";
}

# --- hash ---
{
    my %a = a => 1, b => 2;
    my %b = a => 6, b => 7;
    my %r = %a >>[&csta]<< %b;
    is-deeply %r, {a => "foo", b => "foo"}, "hash >>[&csta]<< returns results";
    is-deeply %a, {a => "foo", b => "foo"}, "hash mutating op writes back";
}
{
    my %a = a => 1, b => 2;
    my %b = a => 6, b => 7;
    my %r = %a >>[&cst]<< %b;
    is-deeply %r, {a => "foo", b => "foo"}, "hash >>[&cst]<< returns results";
    is-deeply %a, {a => 1, b => 2}, "hash non-mutating op leaves lvalue unchanged";
}

# --- hash, lexical code-ref ---
{
    my &metaop = &csta;
    my %a = a => 1, b => 2;
    my %b = a => 6, b => 7;
    my %r = %a >>[&metaop]<< %b;
    is-deeply %a, {a => "foo", b => "foo"}, "hash lexical mutating op writes back";
}
