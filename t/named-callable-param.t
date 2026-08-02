use Test;

# A `&`-sigil NAMED parameter (`:&cb`) binds like any other named parameter.
# It used to bind nothing at all: the parameter was recorded without its sigil,
# every named-key derivation stripped only `@`/`%`, and the body's `&cb` read
# resolved through the env, which a slot-only bind never reaches. The parameter
# therefore always looked unpassed -- and `f() if &f` guards silently skipped.

plan 15;

sub takes-cb(:&cb) { &cb.defined ?? cb() !! 'unpassed' }
is takes-cb(:cb({ 'called' })), 'called', 'a :&cb named parameter binds';
is takes-cb(), 'unpassed', 'an unpassed :&cb is undefined';

# The name may contain a hyphen -- the shape roast's `Test::Tap` uses.
sub takes-hyphen(:&after-tap) { &after-tap.defined ?? after-tap() !! 'unpassed' }
is takes-hyphen(:after-tap({ 'ran' })), 'ran', 'a hyphenated :&name binds';
is takes-hyphen(), 'unpassed', 'an unpassed hyphenated :&name is undefined';

# `key => &blk` is the same binding as `:key(&blk)`.
my &blk = { 'explicit' };
is takes-cb(cb => &blk), 'explicit', 'a fat-arrow named Callable binds';

# The guard idiom that made this invisible: `cb() if &cb`.
sub guarded(:&cb) { my $n = 0; $n = cb() if &cb; $n }
is guarded(:cb({ 7 })), 7, 'the `if &cb` guard sees a passed Callable';
is guarded(), 0, 'the `if &cb` guard skips an unpassed Callable';

# Several `&` named parameters coexist, and mix with other sigils. (The names
# avoid `emit`/`done`, which roast's `Test::Tap` uses: a lexical `&emit` does
# not yet shadow the builtin of that name -- an unrelated, pre-existing gap,
# see todo/tickets/code-lexical-does-not-shadow-a-builtin.md.)
sub several($x, :&pre, :&post, :$live = False, :@extra) {
    my @out = $x, $live, @extra.elems;
    @out.push(pre()) if &pre;
    @out.push(post()) if &post;
    @out
}
is-deeply several(1), [1, False, 0], 'no named arguments at all';
is-deeply several(1, :pre({ 'e' })), [1, False, 0, 'e'],
  'one of several `&` named parameters';
is-deeply several(1, :pre({ 'e' }), :post({ 'd' }), :live, :extra[9]),
  [1, True, 1, 'e', 'd'], 'all of them together';

# A `&` named parameter with a default.
sub defaulted(:&cb = { 'fallback' }) { cb() }
is defaulted(), 'fallback', 'a :&cb default applies when unpassed';
is defaulted(:cb({ 'given' })), 'given', 'a passed :&cb beats its default';

# Multi-dispatch must accept the named `&` argument rather than reject the
# candidate as taking an unexpected named argument.
proto sub disp(|) { * }
multi sub disp($x, :&cb) { &cb.defined ?? cb() !! 'no cb' }
is disp(1, :cb({ 'multi' })), 'multi', 'a multi candidate accepts a `&` named argument';
is disp(1), 'no cb', 'the same candidate without it';

# A surplus named argument is still rejected.
sub strict-named(:&cb) { 1 }
dies-ok { strict-named(:nope({ 1 })) }, 'an unknown named argument still dies';

# vim: expandtab shiftwidth=4
