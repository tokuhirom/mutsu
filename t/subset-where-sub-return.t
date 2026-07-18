use Test;

plan 6;

# A subset whose `where` block calls a `my sub` used to clobber the
# per-frame non-local-return target (`__mutsu_callable_id`) via the slow-path
# scalar env writeback: a block passed to a helper from a multi dispatched
# through such a subset then failed its `return` with
# "Attempt to return outside of any Routine" (JSON::Unmarshal 060).

my sub ident(Mu \obj) { obj }

subset PosThing of Positional where { ident($_) ~~ Positional };

my sub ctx(&code) { code() }

multi unm(Str:D $json, PosThing $obj, *%c) {
    ctx { return 42; }
    return 0;
}

is unm("x", Array[Int]), 42, 'block return escapes helper inside subset-dispatched multi';

# The where-block itself with closure predicates only (no my sub) keeps working.
subset PosSimple of Positional where { True };
multi unm2(Str:D $json, PosSimple $obj) { ctx { return 7; }; return 0; }
is unm2("x", Array[Int]), 7, 'plain-where subset multi block return';

# Nested-return shape: inner multis with their own returns must not corrupt
# the outer routine's return target.
my sub helper(Mu \obj) is pure is raw { obj }
subset ClassLikeT of Mu where -> Mu \type { helper(type).^name.chars > 0 };

class Dog { has Str $.name; }

multi inner(Any:D $json, Mu $t) {
    if $json ~~ Hash {
        return Dog.new(|$json.Hash);
    }
    return $json;
}

multi outer(Str:D $json, PosThing $obj, *%c) {
    ctx {
        my Any \data = [{name => "Roger"}, {name => "Panda"}];
        if data ~~ Positional {
            return @(inner($_, $obj.of) for @(data));
        } else {
            fail "nope";
        }
    }
}

my @dogs = outer("x", Array[Dog]);
is @dogs.elems, 2, 'returned list has both elements';
is @dogs[0].name, "Roger", 'first element constructed';
is @dogs[1].name, "Panda", 'second element constructed';

# ClassLike-style Mu subset dispatch still resolves.
multi tagof(ClassLikeT $t) { $t.^name }
is tagof(Dog), "Dog", 'Mu-based subset with my-sub where dispatches';
