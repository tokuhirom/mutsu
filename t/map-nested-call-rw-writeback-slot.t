use Test;

# A sub with plain `@`/`%` container params registers an exit-time rw-writeback
# keyed by the CALLER frame's compiled slot index. A nested mut-method call in
# the body (`@ctx.map(...)`, a CallMethodMut) drains that still-pending writeback
# early; the caller-frame slot index could collide with THIS frame's `locals`
# range and clobber an unrelated same-index local — e.g. a `%val` Hash param
# overwritten by the `@context` Array. (Template::Mustache `format(%val,@context)`
# hit this via `for @context.map({visit($^ctx,...)}) { $ctx ~~ Promise }`.)

plan 3;

sub get(@context, %val) {
    # Nested mut-method call whose block uses a placeholder + a smartmatch,
    # forcing the interpreter map fallback and then a topic-writeback op.
    for @context.map({ $^ctx }) -> $ctx {
        my $ = ($ctx ~~ Int);
    }
    # %val must still be the Hash we were passed, not @context's Array.
    return %val<val>;
}

my %val = type => 'section', val => 'boolean';
my @context = [ (b => True), ];

is get(@context, %val), 'boolean', "container param not clobbered by nested map";
is %val.^name, 'Hash', "caller %val stays a Hash after the call";
is @context.^name, 'Array', "caller \@context stays an Array after the call";
