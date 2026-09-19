use Test;

# A `method new` that defers with callsame/callwith must resolve against its
# OWN dispatcher — Mu.new is always the final candidate of a `new` MRO — and
# never against a dispatcher an enclosing routine left live. In Raku a plain
# `sub` may see an enclosing multi's dispatcher, but a METHOD call always
# establishes one of its own, so the leak is only ever wrong.
#
# Came from Selkie::UI (ecosystem): every widget constructor is
# `method new(*%args) { %args<focusable> //= False; callwith(|%args) }`, and
# the test files call them from inside `subtest`, a multi sub. `callwith` then
# resolved against `subtest`'s candidate list and died with
# "Cannot resolve caller subtest(Bool:D, Str:D)".

plan 7;

class Widget {
    has Bool $.focusable = True;
    has Str  $.label;
}
class Spinner is Widget {
    method new(*%args --> Spinner) {
        %args<focusable> //= False;
        callwith(|%args);
    }
}

# Baseline: no enclosing dispatcher at all.
{
    my $s = Spinner.new(:label<plain>);
    isa-ok $s, Spinner, 'callwith in `new` reaches Mu.new with no enclosing routine';
    is $s.focusable, False, 'the overridden named arg survives';
}

# A plain sub between the caller and the constructor changes nothing.
sub plain-wrapper(&code) { code() }
{
    my $s = plain-wrapper({ Spinner.new(:label<sub>) });
    isa-ok $s, Spinner, 'callwith in `new` works under a plain sub';
}

# The regression: a MULTI sub frame is live while `new` runs.
multi sub multi-wrapper($desc, &code) { code() }
multi sub multi-wrapper(&code, $desc) { code() }
multi sub multi-wrapper(Pair $p)      { multi-wrapper($p.key, $p.value) }
{
    my $s = multi-wrapper('desc', { Spinner.new(:label<multi>) });
    isa-ok $s, Spinner, 'an enclosing multi sub does not capture `new`\'s deferral';
    is $s.label, 'multi', 'the constructor still received its arguments';
}

# Reached through a Pair-dispatched multi, exactly as `subtest 'x' => { ... }` does.
{
    my $s = multi-wrapper('desc' => { Spinner.new(:label<pair>) });
    isa-ok $s, Spinner, 'nested multi dispatch does not capture `new`\'s deferral either';
}

# And directly in a multi body, with no block in between.
multi sub build-it($d, &c) { Spinner.new(:label<direct>) }
{
    my $s = build-it('desc', { 1 });
    isa-ok $s, Spinner, 'a `new` called straight from a multi body still defers to Mu.new';
}
