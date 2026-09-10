use Test;

# A punned role's attributes live in `__mutsu_attr__*` mixin markers rather than
# the inner instance's attribute map: a bare-role `.new` returns
# `Mixin(empty-instance, {__mutsu_attr__x => ...})`. `.clone` fell through to the
# generic instance clone, which unwraps to the (empty) inner value and drops every
# marker — so the clone lost ALL attributes. Regression: zef's `Zef::Client.fetch`
# does `$candi.clone(:$dist)` on a `Zef::Candidate` (a punned role), and the clone
# then had no `.as`, so `zef fetch` died with "No such method 'as'".

plan 8;

# 1) Punned-role clone preserves non-overridden attributes and applies the override.
{
    role R1 { has $.dist; has Str $.as; has $.uri is rw; }
    my $b = R1.new(dist => "d1", as => "REQ", uri => "u1");
    my $c = $b.clone(dist => "d2");
    is $c.as, "REQ", "punned-role clone keeps a non-overridden attribute";
    is $c.uri, "u1", "punned-role clone keeps a second non-overridden attribute";
    is $c.dist, "d2", "punned-role clone applies the override";
}

# 2) A no-arg clone copies everything, including an rw-mutated attribute.
{
    role R2 { has $.as; has $.uri is rw; }
    my $b = R2.new(as => "REQ", uri => "u1");
    $b.uri = "MUT";
    my $c = $b.clone;
    is $c.uri, "MUT", "no-arg punned-role clone keeps an rw-mutated attribute";
    is $c.as, "REQ", "no-arg punned-role clone keeps a new-time attribute";
}

# 3) The clone is independent of the original (fresh containers).
{
    role R3 { has $.uri is rw; }
    my $b = R3.new(uri => "orig");
    my $c = $b.clone;
    $c.uri = "changed";
    is $b.uri, "orig", "mutating the clone does not affect the original";
}

# 4) A `but Role` mixin over a real class: the role attribute AND the inner
#    class's own attribute both survive, and an override of either works.
{
    role Named { has $.name is rw; }
    class Widget { has $.id; }
    my $w = Widget.new(id => 1) but Named;
    $w.name = "hi";
    my $w2 = $w.clone(id => 99);
    is $w2.name, "hi", "but-mixin clone keeps the role attribute";
    is $w2.id, 99, "but-mixin clone forwards an inner-class attribute override";
}
