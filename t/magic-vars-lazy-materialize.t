use Test;

# Regression pin for todo/tickets/magic-vars-should-be-built-lazily.md
# Slice 2: $*DISTRO/$*PERL/$*RAKU/$*VM/$*KERNEL are no longer eagerly built
# and hoisted into the base tier at Interpreter::new() / thread-clone time;
# they materialize on first read and are cached process-wide thereafter
# (Interpreter::lazy_magic_dynamic_var, src/runtime/io_env.rs). This file
# pins the correctness properties laziness must not disturb: object identity
# stability across repeated reads, cross-thread visibility of the SAME
# cached instance, and the $*PERL !=== $*RAKU distinctness. Definedness
# itself is already pinned by t/base-tier-magic-vars.t and the roast
# KERNEL.t/DISTRO.t/PERL.t/RAKU.t files.
#
# A lazily-built $*RAKU/$*PERL reflecting the compile unit's `use v6.x`
# (make_perl_instance now reads current_language_version() at construction
# time, instead of the old hardcoded Version.new(6) that only became correct
# after an always-eagerly-run update_raku_version_from_parser mutation) is
# pinned as a Rust unit test instead of here:
# `make_perl_instance_version_reflects_current_language_version` in
# src/runtime/io_sysinfo.rs. An `is_run`-based `t/` check for the same
# property (spawning a child mutsu process running `use v6.x; print
# $*RAKU.version`) trips an unrelated, pre-existing fragility specific to
# files under `t/` — see todo/deep/is-run-after-raku-read-swallows-child-spawn.md.

plan 8;

# Repeated reads return the identical cached object, not a fresh build each
# time (the process-wide OnceLock cache must materialize once).
ok $*KERNEL === $*KERNEL, '$*KERNEL read twice is the same cached object';
ok $*DISTRO === $*DISTRO, '$*DISTRO read twice is the same cached object';
ok $*VM === $*VM,         '$*VM read twice is the same cached object';

# $*PERL and $*RAKU are historically distinct objects even though both are
# built by the same make_perl_instance() body / cached_*_instance() OnceLock
# pair.
nok $*PERL === $*RAKU, '$*PERL and $*RAKU are distinct cached objects';

# The cached instance is visible (same identity) across a thread boundary.
my $outer_kernel = $*KERNEL;
my $p = start { $*KERNEL }
ok await($p) === $outer_kernel, '$*KERNEL inside a thread is the same cached object';

# Method-call reads (not just bare-var reads) resolve through the same lazy
# path.
ok $*DISTRO.name.chars > 0, '$*DISTRO.name resolves via a method call read';
ok $*VM.name.chars > 0,     '$*VM.name resolves via a method call read';

# .version, a common access pattern, also resolves correctly in-process
# (without spawning a child).
ok $*RAKU.version.defined, '$*RAKU.version resolves via a method call read';
