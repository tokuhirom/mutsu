use Test;

plan 5;

class PDRegFoo {
    has $.count is rw;
    method inc { $.count++ }
}

my @objs = (1..3).map({ PDRegFoo.new(count => $_) });
@objs».inc;
is @objs.map({ .count }).join(","), "2,3,4", 'hyper dispatch updates rw public attributes';

is-deeply @objs».?missing, [Any, Any, Any], 'hyper .? returns Any type objects when method is missing';

class PDRegMulti {
    has $.v;
    multi method mul(Int $x) { $.v * $x }
    multi method mul(Real $x) { $.v * $x.Int + 100 }
}

my @m = (1..2).map({ PDRegMulti.new(v => $_) });
is-deeply @m».*mul(2), ([2, 102], [4, 104]), 'hyper .* returns all matching method candidates';
is-deeply @m».+mul(2), ([2, 102], [4, 104]), 'hyper .+ returns all matching method candidates';

is (a => 1, a => 2)>>.<a>, '1 2', 'hyper >>.<key> works on pairs';
