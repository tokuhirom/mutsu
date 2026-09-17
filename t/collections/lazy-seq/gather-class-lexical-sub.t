use Test;

# Concurrent::Trie's `entries` method returns a `gather` whose body calls
# `entry-walk`, a `sub` lexical to the class body (declared textually AFTER
# `entries`, which raku allows via hoisting). The gather is pulled outside
# the class entirely (`.list` from mainline code), long after the `entries`
# call's own frame -- and hence its `current_package` -- has returned. That
# deferred pull died with "Unknown function: entry-walk" because the inline
# bytecode exec that resumes a suspended gather coroutine never restored the
# package the body was WRITTEN in (it only restored the compunit, which is
# what `t/modules/gather-module-lexical-helper.t` pins for a top-level `my
# sub`; a class-body lexical sub needs the class's own package instead).

plan 2;

class Doubler {
    method values(*@ns) {
        gather {
            for @ns {
                take helper($_);
            }
        }
    }
    sub helper($n) {
        $n * 2;
    }
}

my $seq = Doubler.new.values(1, 2, 3);
is $seq.list, (2, 4, 6), 'gather body reaches a class-body lexical sub declared after it';

# The sub declared BEFORE the method must resolve too -- this isn't about
# forward-reference hoisting, it is about the package restore itself.
class DoublerBefore {
    sub helper($n) {
        $n * 2;
    }
    method values(*@ns) {
        gather {
            for @ns {
                take helper($_);
            }
        }
    }
}

is DoublerBefore.new.values(1, 2, 3).list, (2, 4, 6),
    'gather body reaches a class-body lexical sub declared before it';
