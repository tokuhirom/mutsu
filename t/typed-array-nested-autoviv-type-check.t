use Test;

# Descending a second subscript through an element that does not exist
# autovivifies an Array/Hash *into* that element. A typed container refuses it:
#
#   my Int @a; @a[0][1] = 5;
#   # Type check failed for an element of @a[0]; expected Int but got Array ([])
#
# mutsu accepted it silently and left `[[(Int) 5]]`. The hash-rooted twin
# (`my Int %h; %h<a><b> = 5`) already threw; only the array root was missing the
# check. Measured against Rakudo v2026.06 (2026-09-04); raku is the oracle and
# this file passes verbatim under both.

plan 10;

# --- 1. a typed array refuses an autovivified intermediate ------------------
dies-ok { my Int @a; @a[0][1] = 5 },
    "a typed array refuses an autovivified inner Array";
dies-ok { my Int @a; @a[0]<x> = 5 },
    "a typed array refuses an autovivified inner Hash";
dies-ok { my Str @s; @s[2][0] = "x" },
    "the check is not Int-specific";

# --- 2. the message names the element, like rakudo --------------------------
{
    my $msg = "";
    { my Int @a; @a[0][1] = 5; CATCH { default { $msg = .message } } }
    ok $msg.contains('element of @a[0]'),
        'the message names the element slot';
    ok $msg.contains('expected Int'), "the message names the expected type";
}

# --- 3. an UNtyped array still autovivifies --------------------------------
{
    my @a;
    @a[0][1] = 5;
    is @a.raku, [[Any, 5],].raku, "an untyped array still autovivifies an inner Array";
}
{
    my @a;
    @a[0]<x> = 5;
    is @a[0]<x>, 5, "an untyped array still autovivifies an inner Hash";
}

# --- 4. a constraint that ACCEPTS the intermediate is fine -----------------
{
    my Any @a;
    @a[0][1] = 5;
    is @a[0][1], 5, "an `Any` element constraint still autovivifies";
}

# --- 5. an element that already holds a container is descended, not vivified -
{
    my Array @a;
    @a[0] = Array.new;
    @a[0][1] = 5;
    is @a[0][1], 5, "an existing inner container is written through, not re-vivified";
}

# --- 6. the hash-rooted twin keeps dying -----------------------------------
dies-ok { my Int %h; %h<a><b> = 5 },
    "a typed hash still refuses an autovivified inner Hash";
