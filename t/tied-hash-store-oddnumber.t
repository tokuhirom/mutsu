use Test;

plan 8;

# Gap A: X::Hash::Store::OddNumber is constructible from user code with its
# :found / :last accessors (it was known internally but not in the user-visible
# exception registry, so `.new` failed with X::Method::NotFound).
{
    my $e = X::Hash::Store::OddNumber.new(:found(0), :last<a>);
    is $e.found, 0,   'X::Hash::Store::OddNumber.new exposes .found';
    is $e.last,  'a', 'X::Hash::Store::OddNumber.new exposes .last';
    is $e.^name, 'X::Hash::Store::OddNumber', 'right exception type';
    isa-ok $e, Exception, 'is an Exception';
}

# A tied hash whose STORE throws X::Hash::Store::OddNumber on an odd assignment.
role OddThrower does Associative {
    method STORE(*@v) {
        X::Hash::Store::OddNumber.new(:found(@v.elems), :last(@v.tail)).throw
            if @v.elems % 2;
    }
    method AT-KEY($) { Nil }
    method keys()    { () }
}
class OT does OddThrower {}

# Gap B: a `%h = <odd>` on the tied hash must route through STORE even when the
# hash is a lexical captured across a call boundary — exactly the shape of a
# `throws-like { %h = ... }` block, which invokes the block from another sub.
{
    my %h is OT;
    throws-like { %h = "a" }, X::Hash::Store::OddNumber, :found(1), :last<a>,
        'captured tied odd-assign routes through STORE and throws';
}

# The same store at top level (not captured in a block) also routes through STORE.
{
    my %h is OT;
    my $threw = False;
    { %h = "solo"; CATCH { when X::Hash::Store::OddNumber { $threw = True } } }
    ok $threw, 'top-level tied odd-assign routes through STORE and throws';
}

# A tied hash with a real backing store: `%h = key/value list` populates it via
# STORE, both top-level and captured in a sub.
role Backed does Associative {
    has %!h;
    method STORE(*@pairs) { %!h = @pairs.map({ .key => .value }).Hash }
    method AT-KEY($k)     { %!h.AT-KEY($k) }
    method keys()         { %!h.keys }
}
class B does Backed {}
{
    my %h is B;
    sub runit(&code) { code() }
    runit({ %h = (x => 1, y => 2) });
    is %h<x>, 1, 'captured store populates backing (x)';
    is %h<y>, 2, 'captured store populates backing (y)';
}
