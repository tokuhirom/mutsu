use Test;

# Found via the `Graph` distribution's ecosystem suite
# (`Graph::Circulant.new(n => 10, jump => 4)` recursing into its own
# positional `new`, and `Graph::Star.new(n => 5, prefix => 'x')`): a `multi
# method` inherited from a parent class must not out-rank a more-derived,
# exactly-matching candidate just because the PARENT candidate happens to
# declare an unrelated sigilled *named* parameter (`:%stuff`, `:@vertexes`).
#
# `pick_method_winner`'s narrowness tie-break counted a sigilled named param
# the same as a sigilled positional one (`(@x, @y)` narrower than `($a, $b)`,
# t/oo/method/multi-method-sigil-narrowness.t) — but a NAMED parameter's type
# decides only whether a candidate is *applicable*, never how narrow it is
# (the same rule `candidate_specificity_rank_for_args` already applies to
# plain multi-sub dispatch). An ancestor class with no matching named
# candidate at all still "matched" through its implicit `*%_` slurpy, and
# then out-ranked the correct, more-derived candidate purely because its
# OWN, unrelated named params happened to carry sigils.

plan 2;

class Base {
    has %.stuff;
    submethod BUILD(:%!stuff = %()) { }
    multi method new(:%stuff = %(), :$foo = Whatever) {
        self.bless(:%stuff);
    }
}

class Derived is Base {
    has Int:D $.n is required;
    submethod BUILD(:$!n!) { }
    multi method new(Int:D $n, Str:D :$prefix = '') {
        self.bless(:$n);
    }
    multi method new(Int:D :$n, Str:D :$prefix = '') {
        self.new($n, :$prefix);
    }
}

is Derived.new(n => 10).n, 10,
    'a named-arg constructor call reaches the more-derived positional candidate, not the ancestor';

# A second class shape, matching Graph::Star more closely: the ancestor
# candidate's sigilled named params (`@`/`%`) must not out-narrow a
# more-derived candidate whose actual matching named param has no sigil.
class Ancestor {
    multi method new(:@vertexes = [], :%adjacency-map = %()) {
        self.bless;
    }
}

class Leaf is Ancestor {
    has Int:D $.n is required;
    submethod BUILD(:$!n!) { }
    multi method new(Int:D :$n) {
        self.bless(:$n);
    }
}

is Leaf.new(n => 3).n, 3,
    'a plain-named more-derived candidate beats an ancestor with sigilled named params';

done-testing;
