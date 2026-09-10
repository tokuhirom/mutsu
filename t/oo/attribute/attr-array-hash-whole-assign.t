# Whole-container assignment to an array/hash private/public attribute twigil
# (`@!a = (...)`, `%!h = (...)`, `@.a = (...)`, `%.h = (...)`) inside a method.
#
# A scalar attribute is parsed sigil-stripped to the local `!x` and mirrors to
# self's shared cell via the SetLocal path; an array/hash attribute keeps its
# `@`/`%` sigil and is stored through SetGlobal, which previously never mirrored
# the new container into the cell — so the write was silently lost (a same-method
# `@!a` read goes cell-direct and saw the unchanged default).
use Test;

plan 12;

# --- whole-array assign to a private attribute ---
{
    class A {
        has @.list;
        method seed { @!list = (1, 2, 3) }
        method seed-from(@src) { @!list = @src }
        method inside { @!list = (7, 8); @!list.elems }
    }
    my $a = A.new;
    $a.seed;
    is $a.list.join(','), '1,2,3', 'whole-array assign via @!list = (...)';

    my $b = A.new;
    $b.seed-from([4, 5, 6]);
    is $b.list.join(','), '4,5,6', 'whole-array assign from a param';

    my $c = A.new;
    is $c.inside, 2, 'same-method read after @!list assign sees the new value';
    is $c.list.join(','), '7,8', 'value persists after the method returns';
}

# --- whole-hash assign to a private attribute ---
{
    class H {
        has %.map;
        method seed { %!map = (a => 1, b => 2) }
        method inside { %!map = (x => 9); %!map.elems }
    }
    my $h = H.new;
    $h.seed;
    is $h.map<a>, 1, 'whole-hash assign via %!map = (...): key a';
    is $h.map<b>, 2, 'whole-hash assign via %!map = (...): key b';
    is $h.map.elems, 2, 'whole-hash assign element count';

    my $h2 = H.new;
    is $h2.inside, 1, 'same-method read after %!map assign sees the new value';
}

# --- public twigil (@.a / %.h) still works ---
{
    class P {
        has @.nums;
        has %.dict;
        method f { @.nums = (10, 20); %.dict = (k => 1) }
    }
    my $p = P.new;
    $p.f;
    is $p.nums.join(','), '10,20', 'whole-array assign via @.nums = (...)';
    is $p.dict<k>, 1, 'whole-hash assign via %.dict = (...)';
}

# --- assign then mutate (push) keeps working together ---
{
    class M {
        has @.items;
        method build { @!items = (1, 2); @!items.push(3) }
    }
    my $m = M.new;
    $m.build;
    is $m.items.join(','), '1,2,3', 'assign followed by push on the same attribute';
}

# --- reassigning replaces (not appends) ---
{
    class R {
        has @.v;
        method twice { @!v = (1, 2, 3); @!v = (9) }
    }
    my $r = R.new;
    $r.twice;
    is $r.v.join(','), '9', 'a second whole-array assign replaces the first';
}
