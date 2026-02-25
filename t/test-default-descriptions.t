use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 4;

{
    my %r = get_out('use Test; plan 1; isa-ok 42, Int;');
    is %r<status>, 0, 'isa-ok script succeeds';
    ok %r<out>.contains("ok 1 - The object is-a 'Int'"), 'isa-ok default description is emitted';
}

{
    my %r = get_out('use Test; plan 2; does-ok 42, Int; can-ok 42, "Str";');
    ok %r<out>.contains("ok 1 - The object does 'Int'"), 'does-ok default description is emitted';
    ok %r<out>.contains("ok 2 - The object can 'Str'"), 'can-ok default description is emitted';
}
