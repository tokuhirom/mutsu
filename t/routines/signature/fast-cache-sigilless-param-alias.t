use v6;
use Test;

# The `fast_method_cache` monomorphic dispatch cache (try_populate_fast_cache,
# vm_call_method_compiled_cache.rs) used to have no eligibility check for a
# sigilless raw-capture parameter (`\x`), which binds the ARGUMENT'S OWN
# CONTAINER rather than a copy of its value. A constructor that threads such
# a parameter into a `:=`-bound attribute (`method !SET-SELF (\v) { $!x := v;
# self }`) built an instance whose attribute silently stopped tracking the
# caller's container from the SECOND (cache-hit) call onward -- only the
# first call's alias survived.
#
# This was invisible on the non-mut `CallMethod` entry (nothing in the
# existing call graph reached a fast-cache hit for this shape there) and was
# only exposed when the sibling `CallMethodMut` entry started consulting the
# same cache (#8880's PR): `Type.new: $x` on a bareword/type-object receiver
# always compiles to `CallMethodMut`.

plan 4;

class Box {
    has $!v;
    method !SET-SELF (\val) { $!v := val; self }
    method new       (\val) { self.bless!SET-SELF: val }
    method bump { $!v++ }
}

my @seen;
for 1..3 -> $n {
    my $x = 0;
    my $b = Box.new: $x;
    $b.bump;
    $b.bump;
    @seen.push($x);
}
is @seen.join(","), "2,2,2",
    "a raw-capture ctor param keeps aliasing the caller's container across cache hits";

# The Iterator-protocol shape the bug was originally found in (roast
# S32-list/skip.t): a custom Iterator whose private SET-SELF binds an
# attribute to a raw-captured outer variable, pulled repeatedly by the
# runtime's own iteration machinery.
{
    my class TimesIterator does Iterator {
        has $!times;
        method !SET-SELF (\times) { $!times := times; self }
        method new       (\times) { self.bless!SET-SELF: times }
        method pull-one {
            $_ < 3 and .return given $!times++;
            IterationEnd
        }
    };

    my @results;
    for 1..2 {
        my $times = 0;
        .sink given (Seq.new(TimesIterator.new: $times), <a b c>)
            .map(-> $a, $b { |(|$a, |$b) })
            .skip;
        @results.push($times);
    }
    is @results.join(","), "4,4", "custom Iterator's bound attribute stays live across repeated construction";
}

# A plain (non-raw) constructor parameter is unaffected -- this must still
# take the fast path and stay correct.
{
    class Plain {
        has $.v;
        method new2($v) { self.new(v => $v) }
    }
    my @out = (1, 2, 3).map({ Plain.new2($_).v });
    is @out.join(","), "1,2,3", "an ordinary (non-sigilless) ctor param is unaffected";
}

# A method with BOTH a plain and a sigilless param: the sigilless one alone
# must be enough to exclude the whole method from the fast cache.
{
    class Mixed {
        has $!a;
        method set(\raw, $plain) { $!a := raw; $plain }
        method get { $!a }
    }
    my @out;
    for 1..2 {
        my $x = 0;
        my $m = Mixed.new;
        $m.set($x, "ignored");
        $x = 5;
        @out.push($m.get);
    }
    is @out.join(","), "5,5", "a mixed sigilless+plain param list still excludes the fast cache";
}
