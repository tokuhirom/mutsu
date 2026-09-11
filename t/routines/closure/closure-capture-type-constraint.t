use v6;
use Test;

# A closure's captured env carries the `__mutsu_type::<name>` metadata that
# enforces a typed lexical's constraint. That metadata is *shadow meta*: it is
# only ever observable through a read or write of its subject, so the capture
# filter keeps it on exactly the same terms as the subject itself (#7565) --
# dropping it for every typed lexical the closure provably cannot name.
#
# These are the shapes where the subject IS captured, so the constraint must
# still bite inside the closure.

plan 12;

sub type-error(&code) {
    my $hit = '';
    try {
        code();
        CATCH { default { $hit = .^name } }
    }
    $hit;
}

# 1-2: a plain typed lexical, captured as an ordinary free variable.
my Int $scalar = 5;
my $write-scalar = sub { $scalar = 'nope' };
is type-error($write-scalar), 'X::TypeCheck::Assignment',
    'captured typed scalar keeps its constraint inside a closure';
is $scalar, 5, 'the rejected assignment left the value alone';

# 3: a typed DYNAMIC. Its env key is a system name, so it is captured without
# ever being a free variable -- the case a naive "keep the metadata only for
# free variables" filter would lose.
my Int $*dyn = 1;
my $write-dyn = sub { $*dyn = 'nope' };
is type-error($write-dyn), 'X::TypeCheck::Assignment',
    'captured typed dynamic keeps its constraint inside a closure';

# 4: a typed array, whose element check is driven by the same metadata lane.
my Int @arr;
my $push-arr = sub { @arr.push('str') };
is type-error($push-arr), 'X::TypeCheck::Assignment',
    'captured typed array keeps its element constraint inside a closure';

# 5: a typed hash.
my Int %hash;
my $store-hash = sub { %hash<k> = 'str' };
is type-error($store-hash), 'X::TypeCheck::Assignment',
    'captured typed hash keeps its value constraint inside a closure';

# 6: the subject is a free variable of a NESTED closure only. The free-var set
# a capture is filtered by covers nested closures, so the metadata must ride
# along with it.
my Int $nested = 3;
my $outer = sub {
    my $inner = sub { $nested = 'nope' };
    inner-call($inner);
};
sub inner-call(&c) { c() }
is type-error($outer), 'X::TypeCheck::Assignment',
    'typed lexical reached only through a nested closure keeps its constraint';

# 7: a closure created inside a routine, capturing that routine's own typed
# lexical, and escaping it.
sub make-setter() {
    my Int $inside = 0;
    return sub ($v) { $inside = $v; $inside };
}
my $setter = make-setter();
is $setter(7), 7, 'escaping closure over a routine-local typed lexical works';
is type-error({ $setter('str') }), 'X::TypeCheck::Assignment',
    'escaping closure keeps the routine-local constraint';

# 8: a typed lexical the closure never names must not constrain a same-named
# lexical the closure declares for itself.
my Int $shadowed = 1;
my $shadow = sub { my $shadowed = 'free'; $shadowed };
is $shadow(), 'free', 'a closure-local redeclaration is not constrained by the outer typed name';

# 9: ... nor by one declared in a routine the closure is created inside.
sub make-shadower() {
    my Int $only-here = 1;
    return sub { my $only-here = 'free'; $only-here };
}
is make-shadower()(), 'free',
    'a closure-local redeclaration is not constrained by the creating routine typed name';

# 10: an untyped capture stays untyped even when a typed lexical of another
# name is in scope right next to it.
my Int $typed-neighbour = 1;
my $plain = 'anything';
my $touch-plain = sub { $plain = 42; $plain };
is $touch-plain(), 42, 'an untyped captured lexical is unaffected by a typed neighbour';

# 11: a typed parameter's constraint is registered on the callee frame and must
# reach a closure created in that frame.
sub typed-param(Int $p is copy) {
    return sub { $p = 'nope' };
}
is type-error(typed-param(1)), 'X::TypeCheck::Assignment',
    'closure over a typed parameter keeps the parameter constraint';
