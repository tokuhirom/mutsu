use v6.e.PREVIEW;
unit module PrecompRevisionProbe;

# Evaluated while this module's mainline runs, so it reports the revision the
# module was compiled under -- 6.e puts the sign before the radix prefix
# (`-0x100`), 6.d renders `0x-100`. A precomp cache hit skips this module's
# parse, so unless the cached entry replays the revision the parse selected,
# this reads the *importer's* revision instead.
our $revision-probe is export = sprintf('%#x', -256);

# A duplicated trait makes the parse emit a warning, which a cache hit must
# replay too.
sub precomp-probe-hello() is export is export { 'hello' }
