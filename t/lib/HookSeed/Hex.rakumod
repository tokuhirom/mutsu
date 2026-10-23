use HookSeed;
use HookSeed::Indexed;
class HookSeed::Hex is HookSeed {
    # A method holding a `gather` keeps the caller's frame env, which carries
    # the declaration seed of every key the `EXPORT` hook returned.
    method named-in-gather {
        my @c = gather { take 1 };
        HookSeed::Indexed.^name
    }
    method named-plain { HookSeed::Indexed.^name }
    method isa-in-gather {
        my @c = gather { take 1 };
        HookSeed::Indexed ~~ HookSeed
    }
}
