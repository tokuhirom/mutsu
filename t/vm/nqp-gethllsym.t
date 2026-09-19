use v6;
use Test;
use nqp;

# `nqp::gethllsym`/`nqp::bindhllsym` were entirely unimplemented ("Unsupported
# nqp:: op: nqp::gethllsym"), which blocked loading Rakudo::CORE::META (a
# dependency of the ecosystem's CLI::Ecosystem 0.0.7): its BEGIN block reads
# `nqp::gethllsym("default","SysConfig").rakudo-build-config<version>` before
# any Raku-level class is even composed. Issue #8775.

plan 6;

# Real Rakudo installs `Perl6::SysConfig` under `("default", "SysConfig")`
# during its own compiler bootstrap; mutsu seeds the same binding so this
# BEGIN-time lookup resolves without a special case for the one distribution
# that needs it.
my $sys-config := nqp::gethllsym("default", "SysConfig");
is $sys-config.^name, 'Perl6::SysConfig',
    'gethllsym("default", "SysConfig") answers a Perl6::SysConfig instance';

my $build-config := $sys-config.rakudo-build-config;
isa-ok $build-config, Map, '.rakudo-build-config is Map-shaped';
ok $build-config<version>.defined, '.rakudo-build-config<version> is defined';

# Genuinely general, not a special case for "default"/"SysConfig": a value
# bound under an arbitrary HLL/name pair round-trips.
nqp::bindhllsym('default', 'MyTestSymbol', 42);
is nqp::gethllsym('default', 'MyTestSymbol'), 42,
    'bindhllsym/gethllsym round-trip an arbitrary symbol';

nqp::bindhllsym('nqp', 'AnotherSymbol', 'hello');
is nqp::gethllsym('nqp', 'AnotherSymbol'), 'hello',
    'symbols are keyed by HLL, not just by name';

# An unbound (hll, name) pair is absent -- mutsu has no separate native-null
# representation from Nil (the same simplification `nqp::ifnull` documents),
# so it comes back false/undefined rather than real NQP's raw VMNull.
nok nqp::gethllsym('default', 'NoSuchSymbolEver').defined,
    'gethllsym on an unbound symbol is not defined';
