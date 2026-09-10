use Test;

# ADR-0058 §9.4. `.map` on a real `@` array is deferred like every other
# spelling now, which put four more pure-value read paths in front of a
# not-yet-run `SeqSource::MapGrep` body. Each row below answered nothing (or
# the wrong thing) because the reader saw ADR-0034's empty seed, or because
# the pull no longer happens in the frame that created the callback.
#
# Every assertion was verified by running this exact file under real `raku`,
# which passes it 8/8.

plan 8;

class Outer {
    class Inner { has Str $.name }
    our sub make-inners(@names) { @names.map(-> $name { Inner.new(:$name) }) }
}

# 1-2. String interpolation. `StringConcat` is the one string path with no
# surrounding coercion op to hang the reify guard on, so `"{ ... }"` rendered
# an unpulled Seq as the empty string.
{
    my @a = 1, 2, 3;
    is "X{ @a.map({ $_ * 2 }) }Y", "X2 4 6Y", 'a deferred .map interpolates its elements';
    is "Z{ map { $_ * 2 }, @a }W", "Z2 4 6W", '... and so does the listop spelling';
}

# 3. A recursive producer through interpolation (roast/S06-signature/positional.t).
{
    sub f(@a, $i) { $i ~ "[{ map { f($_, $i + 1) }, @a }]" }
    is f([[], [[],], []], 0), "0[1[] 1[2[]] 1[]]", 'a recursive interpolated map terminates';
}

# 4-5. A slice INDEX that is itself a deferred Seq: raku evaluates the index
# sequence first, so the store must pull it rather than address no slots.
{
    my @n = 2, 3;
    @n[@n.map(* + 0)] = <a b>.sort;
    is-deeply @n, [2, 3, "a", "b"], 'a deferred .map used as a slice index is pulled';

    my @m = 2, 3;
    @m[@m.List.map(* + 0)] = <a b>.sort;
    is-deeply @m, [2, 3, "a", "b"], '... whichever receiver spelling built it';
}

# 6-7. The callback's captured lexical must win over a same-named lexical live
# in whatever frame consumes the Seq. The pull is no longer inside the frame
# that created the block, so plain caller priority silently resolved the
# block's own free variable to the consumer's.
{
    my @data = 1, 2;
    my $c = "OUTER";
    sub build($t) { my $c = $t; @data.map: { "$c:$_" } }
    sub build-listop($t) { my $c = $t; map { "$c:$_" }, @data }
    is build("A").List.join(","), "A:1,A:2", 'the rw map pull reads the captured lexical';
    is build-listop("B").List.join(","), "B:1,B:2", '... and so does the listop rw pull';
}

# 8. The callback runs under its DECLARING package, so a nested class short
# name still resolves when the Seq is consumed from somewhere else.
{
    my @names = "a", "b";
    is Outer::make-inners(@names).List.map(*.name).join(","), "a,b",
        'the pull resolves nested-class names in the callback\'s own package';
}
