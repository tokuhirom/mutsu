use Test;

# Hyper function-op with an *assignment meta-operator* code-ref (`&[+=]`,
# `&[~=]`, ...) held in a lexical `&` variable writes the result back into the
# left lvalue. A mutating meta-op applied to a non-lvalue dies, and the dwim
# length rules are enforced. Mirrors the patterns in roast/S03-metaops/infix.t.

plan 23;

my &add  = &[+];
my &cat  = &[~];
my &addeq = &[+=];
my &cateq = &[~=];

# --- scalar assignment meta-op ---
{
    my $a = 1;
    my $r = $a >>[&addeq]<< 6;
    is $r, 7, "scalar >>[&addeq]<< returns sum";
    is $a, 7, "scalar assignment meta-op writes back";
}
{
    my $a = "h";
    my $r = $a >>[&cateq]<< "x";
    is $r, "hx", "lexical &[~=] returns concatenation";
    is $a, "hx", "lexical assignment meta-op writes back";
}

# --- array assignment meta-op ---
{
    my @a = 1, 2, 3;
    my @r = @a >>[&addeq]<< (6, 7, 8);
    is-deeply @r, [7, 9, 11], "array >>[&addeq]<< returns sums";
    is-deeply @a, [7, 9, 11], "array assignment meta-op writes back";
}
{
    my @a = 1, 2, 3;
    my @r = @a <<[&addeq]>> 3;     # scalar broadcast over array
    is-deeply @r, [4, 5, 6], "array <<[&metaop]>> scalar broadcasts";
    is-deeply @a, [4, 5, 6], "array broadcast meta-op writes back";
}

# --- hash assignment meta-op ---
{
    my %a = a => 1, b => 2;
    my %b = a => 6, b => 7;
    my %r = %a >>[&addeq]<< %b;
    is-deeply %r, {a => 7, b => 9}, "hash >>[&addeq]<< returns sums";
    is-deeply %a, {a => 7, b => 9}, "hash assignment meta-op writes back";
}

# --- dwim length rules: non-dwim array vs scalar dies ---
{
    my @a = 1, 2, 3;
    dies-ok { @a >>[&add]<< 3 }, ">>op<< array vs scalar dies (non-dwim)";
    dies-ok { @a <<[&add]<< 3 }, "<<op<< array vs scalar dies (left longer)";
    is-deeply (@a <<[&add]>> 3), [4, 5, 6], "<<op>> broadcasts scalar (both dwim)";
    is-deeply (@a >>[&add]>> 3), [4, 5, 6], ">>op>> broadcasts scalar (right dwim)";
}

# --- mutating meta-op on a non-lvalue dies ---
{
    my @a = 1, 2, 3;
    dies-ok { 3 >>[&addeq]<< @a }, "meta-op with literal left dies (non-dwim)";
    dies-ok { 3 <<[&addeq]>> @a }, "meta-op with literal left dies (both dwim)";
    is-deeply (3 <<[&add]>> @a), [4, 5, 6], "plain op with literal left broadcasts";
}

# --- hash dwim/scalar rules ---
{
    my %a = a => 1, b => 2;
    dies-ok { %a >>[&add]<< 3 }, "hash >>op<< scalar dies (non-dwim)";
    is-deeply (%a <<[&add]>> 3), {a => 4, b => 5}, "hash <<op>> scalar broadcasts";
    dies-ok { 3 >>[&add]<< %a }, "scalar >>op<< hash dies (non-dwim)";
    is-deeply (3 <<[&add]>> %a), {a => 4, b => 5}, "scalar <<op>> hash broadcasts";
    dies-ok { 3 <<[&addeq]>> %a }, "meta-op scalar-left over hash dies";
}

# --- mutating user sub (sigilless) on non-lvalue dies naturally ---
{
    sub csta(\a, \b) { a = "foo" }
    my &cs = &csta;
    dies-ok { 3 <<[&cs]>> (1, 2) }, "mutating sigilless sub on literal dies";
}
