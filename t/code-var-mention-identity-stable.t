use v6;
use Test;

plan 8;

# Repeated bareword mentions of a top-level sub must resolve to the SAME
# object identity, matching raku (todo/tickets/code-var-mention-remakes-the-sub.md).
sub f() { 1 }
is &f.WHICH, &f.WHICH, 'repeated &f mentions share a stable .WHICH';
is &f.WHERE, &f.WHERE, 'repeated &f mentions share a stable .WHERE';

# A `.wrap()` installed through one mention must be visible to a direct call
# through a DIFFERENT, later mention of the same routine — not just to calls
# made by bareword name. Before the identity fix, `&f()` (a fresh mention,
# invoked directly as a value) bypassed an active wrap chain even though
# `f()` (a named call) saw it, because the wrap chain was keyed on the
# mention's own (unstable) SubData id.
my @seen;
sub g() { @seen.push('original'); 'orig-result' }
my &h = &g;
&h.wrap(sub () { @seen.push('wrapped'); callsame });
&g();
is-deeply @seen, ['wrapped', 'original'],
    'a wrap chain installed via one &g mention fires for a call through another mention';

# A nested named sub re-materializes fresh per invocation of its enclosing
# routine (raku: verified directly against `raku`), even when it captures
# nothing from the enclosing scope -- the identity fix must NOT collapse
# these into one shared identity.
sub outer($x) {
    my sub inner() { $x }
    return &inner;
}
my $a = outer(1);
my $b = outer(2);
isnt $a.WHICH, $b.WHICH,
    'a nested named sub gets a fresh identity per invocation of its enclosing routine';
is $a(), 1, 'first closure instance still sees its own captured value';
is $b(), 2, 'second closure instance still sees its own captured value';

# But within the SAME invocation, repeated mentions of the nested sub share
# one identity (same rule as the top-level case above).
sub outer2($x) {
    my sub inner2() { $x }
    return (&inner2.WHICH, &inner2.WHICH);
}
my ($w1, $w2) = outer2(5);
is $w1, $w2, 'repeated mentions of a nested sub within one call share identity';

# A class method's bareword-style lookup (`&obj.^find_method(...)`-free path,
# just the plain `.can`-independent .WHICH stability for a sub) stays stable
# too for a simple case: calling the sub twice does not disturb its identity.
sub k() { 42 }
my $id1 = &k.WHICH;
k();
k();
my $id2 = &k.WHICH;
is $id1, $id2, 'calling a sub does not perturb its own &-mention identity';
