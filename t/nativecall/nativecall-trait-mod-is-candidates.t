use Test;

# Rakudo's `NativeCall.rakumod` declares `trait_mod:<is>` candidates for its own
# traits and exports them, so `&trait_mod:<is>.candidates` is introspectable
# after `use NativeCall`. The `NativeLibs` distribution depends on exactly that:
# its custom `sub EXPORT` picks the `:$native!` candidate out of the list and
# re-exports `.dispatcher`, so its own importers get `is native` too.
#
# mutsu applies these four traits natively at declaration time, and used to
# register only a content-free shell under the exported name -- `.candidates`
# answered one entry with no signature, `.first` gave Nil, and the module died
# on `Any.dispatcher`.
plan 4;

use NativeCall;

my $native = &trait_mod:<is>.candidates.first: { .signature ~~ :(Routine, :$native!) };
ok $native.defined, 'NativeCall exports a `trait_mod:<is>` candidate for `is native`';
is $native.signature.gist, '(Routine $r, :$native!)', '...with the signature rakudo declares';

ok &trait_mod:<is>.candidates.first({ .signature ~~ :(Routine, :$symbol!) }).defined,
    '...and one for `is symbol`';

# The whole `NativeLibs` idiom in one line: the re-exported dispatcher must
# still carry every candidate, not just the one that was matched.
ok $native.dispatcher.candidates.elems >= 2,
    'the candidate\'s dispatcher carries the whole multi';
