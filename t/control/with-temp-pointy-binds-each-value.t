use Test;

# `with EXPR -> \d { ... }` binds `d` to EXPR's value afresh every time the
# `with` runs. EXPR's value lives in a compiler temporary's raw slot (#18315);
# binding a container from it (a Hash or Array) must not share the temp by
# name, or every later iteration sees the first one's container (#18377).
# The shapes `with-pointy-sigilless-loop.t` does not cover: nested withs in a
# method, a variable holding the Hash, a scalar pointy, `without`, the
# implicit topic and `andthen`.

plan 10;

my @t = {p => {e => 1}}, {p => {e => 2}}, {p => {e => 3}};

my @seen;
for @t { with .<p> -> \d { @seen.push: d<e> } }
is-deeply @seen, [1, 2, 3], 'a sigilless pointy binds each iteration\'s Hash';

@seen = ();
for @t { with .<p> -> $d { @seen.push: $d<e> } }
is-deeply @seen, [1, 2, 3], 'a scalar pointy binds each iteration\'s Hash';

@seen = ();
for @t { my $tmp = .<p>; with $tmp -> \d { @seen.push: d<e> } }
is-deeply @seen, [1, 2, 3], 'the condition is a variable holding a Hash';

@seen = ();
for [1, 2], [3, 4] { with $_ -> \d { @seen.push: d[1] } }
is-deeply @seen, [2, 4], 'an Array condition';

@seen = ();
for @t {
    with .<p> -> \decl {
        with decl<e> -> \e { @seen.push: e }
    }
}
is-deeply @seen, [1, 2, 3], 'nested withs with sigilless pointies';

class C {
    method m(@s) {
        my @d;
        for @s { with .<a> -> \d { with d<b> -> \e { @d.push: e } } }
        @d
    }
}
is-deeply C.new.m([{a => {b => 1}}, {a => {b => 2}}]), [1, 2], 'inside a method';

@seen = ();
for @t { without .<missing> { @seen.push: 'none' } }
is-deeply @seen, ['none', 'none', 'none'], 'without';

my %h = a => 1;
with %h -> \d { d<b> = 2 }
is %h<b>, 2, 'the bound container is the original, not a copy';

@seen = ();
for @t { with .<p> { @seen.push: .<e> } }
is-deeply @seen, [1, 2, 3], 'the implicit topic';

@seen = ();
for @t { .<p><e> andthen @seen.push: $_ }
is-deeply @seen, [1, 2, 3], 'andthen';
