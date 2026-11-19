use Test;

# The lexical import sets a `use` fills (ADR-8751) are world state shared by
# every thread (#18137). A worker started before a `use` ran (here at BEGIN,
# as a module's mainline does during `use`) used to keep its own copy of the
# sets, taken at its spawn, so a closure calling an imported routine died there
# with "Unknown function".

use lib 't/lib';

my Channel $C;
BEGIN { $C .= new; start { loop { my $s = $C.receive; $s() } } }

sub on-worker(&code) {
    my $p = Promise.new;
    my $v = $p.vow;
    $C.send({ CATCH { default { $v.break($_) } }; $v.keep(code()) });
    await $p
}

use LateImportOnWorker;

plan 4;

is late-greet('main'), 'hello main', 'the import works on main';
is on-worker({ late-greet('worker') }), 'hello worker',
    "a unit-level import is visible on a worker started before the `use`";
is on-worker({ late-kind(1) ~ late-kind('a') }), 'intstr',
    'an imported multi family dispatches on the worker';
{
    use LateImportOnWorkerBlock;
    is on-worker({ block-greet() }), 'from the block',
        "a block's import is visible on the worker";
}
