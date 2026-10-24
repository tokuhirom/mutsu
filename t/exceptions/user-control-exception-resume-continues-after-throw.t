use v6;
use Test;

plan 6;

# A control exception of a user-declared class (one doing `X::Control`) offers
# itself to the CONTROL handlers at its throw site: a handler's `.resume`
# continues right after the `.throw`, even from inside a callee.
class CX::Mine is X::Control {
    has $.name;
    method message { "mine" }
}

sub thrower { CX::Mine.new(:name<f>).throw; "after" }
my @seen;
{
    @seen.push("r: " ~ thrower());
    CONTROL { when CX::Mine { @seen.push("ctl " ~ .name); .resume } }
}
is @seen.join("|"), "ctl f|r: after", "resume from a callee continues after the throw";

class B { method Bool { CX::Mine.new(:name<b>).throw; False } }
my $b = B.new;
@seen = ();
{
    @seen.push("cond: " ~ ($b ?? "t" !! "f"));
    CONTROL { when CX::Mine { @seen.push("ctl " ~ .name); .resume } }
}
is @seen.join("|"), "ctl b|cond: f", "a Bool that throws and is resumed answers its return value";

@seen = ();
{
    @seen.push("and: " ~ ($b && 5).WHAT.^name);
    CONTROL { when CX::Mine { @seen.push("ctl"); .resume } }
}
is @seen.join("|"), "ctl|and: B", "&& short-circuits on the resumed False";

# The handler sees the exception object itself.
my $topic;
{
    CX::Mine.new(:name<topic>).throw;
    CONTROL { when CX::Mine { $topic = .name; .resume } }
}
is $topic, "topic", "the topic is the thrown exception";

# A handler that does not resume ends its region as before.
my $after = "no";
{
    CX::Mine.new(:name<stop>).throw;
    $after = "yes";
    CONTROL { when CX::Mine { } }
}
is $after, "no", "a handler that does not resume unwinds";

# A handler that matches by `default` without resuming also ends its region.
my $escaped = "no";
{
    CX::Mine.new(:name<y>).throw;
    CONTROL { default { $escaped = "control" } }
}
is $escaped, "control", "a `default` handler that does not resume ends the region";
