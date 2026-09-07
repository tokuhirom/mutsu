use v6;
use Test;

# A user-defined tied container (`my %h is OT` where OT does Associative and
# declares STORE) must route `%h = ...` through that STORE, in every position.
#
# Dropping the by-name `is <Type>` capture-cell exclusion put an `is <Type>`
# declaration's value INSIDE a shared ContainerRef cell. Three consumers were
# converted to look through the cell at the time; the TIE DISPATCH was a fourth,
# and it is the one only a user-defined container class reaches --
# `tied_instance_type_name` matched Instance/Mixin but not ContainerRef, so
# every such assignment fell through to the plain-Hash path and raised mutsu's
# own X::Hash::Store::OddNumber instead of calling STORE.
#
# Two things hid it: mutsu's native `throws-like` hard-codes a skip for that
# exception's `.found`/`.last` matchers, and a `CATCH { when
# X::Hash::Store::OddNumber }` matches mutsu's own exception by name just as
# well as the user's. So assert on the STORE actually running.

plan 9;

my @calls;

role Recorder does Associative {
    method STORE(*@v) { @calls.push('STORE(' ~ @v.elems ~ ')'); self }
    method AT-KEY($)  { Nil }
    method keys()     { () }
}
class Rec does Recorder {}

sub invoke(&c) { c() }

{
    @calls = ();
    my %h is Rec;
    %h = "a";
    is @calls.join(','), 'STORE(1)', 'a top-level assignment routes through STORE';
}

{
    @calls = ();
    my %h is Rec;
    { %h = "b" }
    is @calls.join(','), 'STORE(1)', 'an assignment in a bare block routes through STORE';
}

{
    @calls = ();
    my %h is Rec;
    invoke({ %h = "c" });
    is @calls.join(','), 'STORE(1)',
       'a captured assignment across a call boundary routes through STORE';
}

{
    @calls = ();
    invoke({ my %g is Rec; %g = "d" });
    is @calls.join(','), 'STORE(1)',
       'a declaration inside the invoked block routes through STORE';
}

# The tie survives the assignment: `%h` is still the tied container afterwards,
# not the plain Hash the fall-through path would have left.
{
    my %h is Rec;
    %h = "e";
    is %h.^name, 'Rec', 'the container keeps its type across the assignment';
    invoke({ is %h.^name, 'Rec', '... and through a capture' });
}

# The user's own exception reaches the caller with its own attributes, rather
# than mutsu's same-named built-in one. (Caught with `try`/`$!` rather than
# CATCH+LEAVE, which diverges from rakudo for an unrelated reason.)
role OddThrower does Associative {
    method STORE(*@v) {
        X::Hash::Store::OddNumber.new(:found(@v.elems), :last(@v.tail)).throw
            if @v.elems % 2;
    }
    method AT-KEY($) { Nil }
    method keys()    { () }
}
class OT does OddThrower {}
{
    my %h is OT;
    try invoke({ %h = "z" });
    is $!.found, 1, "the thrown exception carries the user's .found";
    is $!.last, 'z', "... and the user's .last";
}

# The built-in container traits the cell conversion was made for still work.
{
    my %b is BagHash = a => 1, b => 2;
    is %b<b>, 2, 'a built-in `is BagHash` declaration still takes its initialiser';
}
