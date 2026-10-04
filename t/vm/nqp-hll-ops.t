use Test;
use nqp;

# The HLL-Specific nqp:: ops and nqp::force_gc (#11504). Expected answers are Rakudo 2026.09's
# except `hlllist` / `hllhash`: Rakudo answers its BOOTArray / BOOTHash VM
# types, mutsu the type of what its own `nqp::list` / `nqp::hash` build (see
# #11553 for the representation decision).

plan 14;

is nqp::hllboxtype_i().^name, 'Int', 'hllboxtype_i';
is nqp::hllboxtype_n().^name, 'Num', 'hllboxtype_n';
is nqp::hllboxtype_s().^name, 'Str', 'hllboxtype_s';

ok nqp::isnull(nqp::getcurhllsym('nqp-hll-ops-unbound')), 'an unbound current-HLL symbol is null';
is nqp::bindcurhllsym('nqp-hll-ops-foo', 42), 42, 'bindcurhllsym answers the value';
is nqp::getcurhllsym('nqp-hll-ops-foo'), 42, 'getcurhllsym reads it back';
is nqp::gethllsym('Raku', 'nqp-hll-ops-foo'), 42, 'the current HLL is Raku';
nqp::bindhllsym('Raku', 'nqp-hll-ops-bar', 7);
is nqp::getcurhllsym('nqp-hll-ops-bar'), 7, 'and getcurhllsym sees bindhllsym("Raku", ...)';

is nqp::hlllist().^name, nqp::list().^name, 'hlllist is the type nqp::list builds';
is nqp::hllhash().^name, nqp::hash().^name, 'hllhash is the type nqp::hash builds';

{
    my $destroyed = 0;
    my class D { submethod DESTROY { $destroyed++ } }
    D.new for ^20;
    ok nqp::isnull(nqp::force_gc()), 'force_gc answers null';
    ok $destroyed > 0, 'force_gc collects, and DESTROY runs';
}

ok nqp::isnull(nqp::sethllconfig('Raku', nqp::hash())), 'sethllconfig answers null';
# (Under Rakudo this switch leaves the program on the compiler's HLL config,
# and `done-testing` then dies "'Raku' doesn't have HLL bools" after every
# test passed; mutsu has one HLL config, so it is a no-op.)
ok nqp::isnull(nqp::usecompilerhllconfig()), 'usecompilerhllconfig answers null';
