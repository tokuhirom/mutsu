use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 8;

# isa-ok default description renders the expected type via its .raku: a type
# object is its bare name, but a Str type-name is quoted.
{
    my %r = get_out('use Test; plan 2; isa-ok 42, Int; isa-ok 42, "Int";');
    ok %r<out>.contains("ok 1 - The object is-a 'Int'"),
        'isa-ok with a type object uses the bare name';
    ok %r<out>.contains(qq{ok 2 - The object is-a '"Int"'}),
        'isa-ok with a Str type-name quotes it';
}

# can-ok default description names the object type and the method.
{
    my %r = get_out('use Test; plan 1; class Womble { method m {...} }; can-ok Womble.new, "m";');
    ok %r<out>.contains("ok 1 - An object of type 'Womble' can do the method 'm'"),
        'can-ok default description';
}

# does-ok default description says "does role".
{
    my %r = get_out('use Test; plan 1; role R {}; class C does R {}; does-ok C.new, R;');
    ok %r<out>.contains("ok 1 - The object does role 'R'"),
        'does-ok default description';
}

# A failing `is` under `todo` emits the expected/got diagnostic as `#`-prefixed
# TAP comments on stdout (routed with the `# Failed test` block), matching raku.
{
    my %r = get_out('use Test; plan 1; todo "later"; is 3, 4, "cmp";');
    ok %r<out>.contains("not ok 1 - cmp # TODO later"), 'todo failure TAP line';
    ok %r<out>.contains("# expected: '4'"), 'todo failure emits # expected on stdout';
    ok %r<out>.contains("#      got: '3'"), 'todo failure emits # got on stdout';
}

# A non-todo `is` failure keeps its diagnostic off stdout (it goes to stderr).
{
    my %r = get_out('use Test; plan 1; is 3, 4, "cmp";');
    nok %r<out>.contains("# expected:"), 'non-todo failure diagnostic is not on stdout';
}

done-testing;
