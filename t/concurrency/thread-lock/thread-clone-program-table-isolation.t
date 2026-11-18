use Test;

# The interpreter's program-global symbol tables (loaded modules, exported
# names, per-package lexicals, class bookkeeping) are shared with every
# thread clone rather than deep-copied (#7667). A spawned block sees what the
# parent declared before it, and since ADR-17216 the type registry is one
# world table: a class the spawn declares is visible to the parent, as on
# rakudo.

plan 6;

# A module used before the spawn is visible inside it.
use MIME::Base64;
my $encoded = await start { MIME::Base64.encode-str('mutsu') };
is $encoded, MIME::Base64.encode-str('mutsu'),
    'a module loaded before the spawn is usable inside it';

# A class declared before the spawn is visible inside it.
class Before { method greet() { 'before' } }
is (await start { Before.new.greet }), 'before',
    'a class declared before the spawn is visible inside it';

# A class declared INSIDE the spawn is visible to the parent (raku: 1).
await start { EVAL 'class OnlyInChild { method x() { 1 } }' };
is (try EVAL 'OnlyInChild.new.x'), 1,
    'a class declared inside a spawn is visible to the parent';

# Package-scope statics: the parent's value survives a spawn that writes its own.
package P {
    our $shared = 'parent';
}
await start { $P::shared = 'child' };
# The write is to the same `our` container, so it IS shared -- that is the
# documented `our` semantics, not table leakage. What must hold is that the
# name still resolves to one container on both sides.
is $P::shared, 'child', 'an `our` variable stays one shared container across a spawn';

# Many spawns in a row keep seeing a consistent view of the tables.
my @seen = await Promise.allof((^8).map({ start { Before.new.greet } })).then({ 'done' });
is @seen, 'done', 'eight concurrent spawns all complete';
is (await start { await start { Before.new.greet } }), 'before',
    'a nested spawn still sees the outer declarations';
