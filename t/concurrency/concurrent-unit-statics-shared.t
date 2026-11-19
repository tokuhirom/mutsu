use Test;

# A module's file-scope lexicals (compunit statics, `unit_lexicals`) are world
# state, shared by every thread (ADR-17216 §2.5, #18136). A worker started
# before the module was loaded used to keep its own copy of the table, taken
# at its spawn, so the module's routines read their statics as Any there.

use lib 't/lib';

plan 4;

my Channel $C .= new;
my $worker = start { loop { my $s = $C.receive; $s() } };

sub on-worker(&code) {
    my $p = Promise.new;
    my $v = $p.vow;
    $C.send({ CATCH { default { $v.break($_) } }; $v.keep(code()) });
    await $p
}

require LateUnitStatics;
my &get = &LateUnitStatics::get;
my &set = &LateUnitStatics::set;

is get(), 'x=42 h=1 n=0', 'main reads the statics';
is on-worker({ get() }), 'x=42 h=1 n=0', 'a worker started before the load reads them too';
on-worker({ set(7) });
is get(), 'x=7 h=7 n=1', "the worker's writes are seen on main";
await start { set(9) };
is on-worker({ get() }), 'x=9 h=9 n=2', "another thread's writes are seen on the worker";
