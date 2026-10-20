use Test;

# `Iterable.lazy-if($flag)` is `$flag ?? self.lazy !! self`; `Any.lazy-if`
# answers the receiver whatever the flag is (#14134).

plan 22;

for List, Array, Map, Hash, Seq, Range, Any, Slip -> \t {
    ok t.^methods.map(*.name).Set<lazy-if>, "{t.^name}.^methods lists lazy-if";
}

is (1, 2).lazy-if(False).raku, (1, 2).raku, "a false flag keeps the list";
is (1, 2).lazy-if(True).raku, (1, 2).lazy.raku, "a true flag makes it lazy";
is [1, 2].lazy-if(1).is-lazy, True, "an Array turns lazy for a true value";
is [1, 2].lazy-if(0).is-lazy, False, "...and not for a false one";
is (1..3).lazy-if(True).raku, (1..3).lazy.raku, "a Range";
is (1..*).lazy-if(False)[^3], (1, 2, 3), "an unbounded Range stays itself";
is {a => 1}.lazy-if(True).is-lazy, True, "a Hash lists lazily";
is <a b>.Seq.lazy-if(True).is-lazy, True, "a Seq";

is 5.lazy-if(True), 5, "Any.lazy-if ignores the flag";
is "a".lazy-if(True), "a", "a Str";
is Any.lazy-if(True).raku, "Any", "the Any type object";
is Set.new(1).lazy-if(True).raku, Set.new(1).raku, "a Set is not made lazy";
is (class Plain { }).new.lazy-if(True).^name, "Plain", "an instance of a user class";
throws-like { Mu.lazy-if(True) }, X::Method::NotFound, "Mu has no lazy-if";
