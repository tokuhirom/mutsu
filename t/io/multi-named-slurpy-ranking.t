use Test;

# A multi candidate that declares a named parameter (optional) plus a named-hash
# slurpy is NARROWER than a bare zero-arg candidate: in Raku a zero-arg method is
# really `(*%_)`, so `(Bool :$b, *%a)` out-narrows it. Regression: mutsu ranked the
# bare `()` candidate as more specific (a flat +2000 distance penalty on the named
# slurpy let `()` win), so `HTTP::Request.new(GET => $url)` dispatched to the wrong
# `new` and dropped the URL.

plan 6;

class C {
    multi method g(Bool :$b, *%a) { "slurpy" }
    multi method g()              { "zero" }
}

is C.g(b => True), "slurpy", "declared named+slurpy beats bare zero-arg (named that binds)";
is C.g(x => 1),    "slurpy", "declared named+slurpy beats bare zero-arg (extra named to slurpy)";
is C.g(),          "slurpy", "declared named+slurpy beats bare zero-arg (no args)";

# The HTTP::Request.new(GET => $url) shape: the slurpy candidate collects the pair.
class Req {
    has $.url;
    proto method new(|) {*}
    multi method new(Bool :$bin, *%args) {
        my $url = 'none';
        for %args.kv -> $k, $v { $url = $v if $k.lc eq 'get' }
        self.bless(:$url)
    }
    multi method new() { self.bless(:url('default')) }
}

is Req.new(GET => "http://example.com/").url, "http://example.com/",
    "GET => url reaches the (Bool :\$bin, *%args) candidate";
# Even with no args, the `(Bool :$bin, *%args)` candidate is narrower than the
# bare `()` (which is really `(*%_)`), so it wins and `%args` is simply empty —
# exactly as rakudo dispatches.
is Req.new.url, "none", "no-arg new still reaches the narrower slurpy candidate (raku parity)";

# A named-hash slurpy still LOSES to a candidate declaring an explicit named param
# (the tie-break that the removed penalty used to short-circuit).
class D {
    multi method f(Int :$file!) { "explicit" }
    multi method f(*%items)     { "slurpy" }
}
is D.f(file => 3), "explicit", "explicit named param beats a named-hash slurpy";
