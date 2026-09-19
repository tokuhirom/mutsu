use v6;
use Test;
use lib 't/lib';
use BlockBacktraceFixture;

# Pin for #8743: a `RoutineFrame`'s call-site `file` used to be the
# dynamically-scoped `?FILE`, which still names the mainline while a `use`d
# module's own routine runs. An inlined bare block (`try { ... }`) records no
# `def_file` of its own (it belongs to its enclosing routine), so its
# backtrace frame fell through to that stale `?FILE` and reported the SCRIPT
# instead of the module the block is actually written in — even though the
# routine that lexically encloses it reported correctly (masked by its own
# `def_file`, which the block frame does not carry).

plan 2;

my $bt = fixture-block-dies();
# The `try { ... }` block itself: an anonymous frame, `.subname` empty.
my $block-frame = $bt.list.first(*.subname eq '');

ok $block-frame.file ~~ /BlockBacktraceFixture\.rakumod/,
    'the inlined block reports the module file';
nok $block-frame.file ~~ /'backtrace-module-block-file'\./,
    'and not the script that called into the module';

done-testing;
