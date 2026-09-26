use v6;
use Test;

# PDF::ISO_32000_2 composes interface roles into a hash at runtime. The role
# requirement must be checked on that value, just as it is for a class header.
plan 3;

role NeedsFoo {
    method foo() {...}
}

dies-ok { %() does NeedsFoo },
    'a runtime role mixin rejects an unimplemented required method';

role ProvidesFoo does NeedsFoo {
    method foo() { 'ok' }
}

my $value;
lives-ok { $value = %() does ProvidesFoo },
    'a concrete method supplied by a composed role satisfies the requirement';
is $value.foo, 'ok', 'the concrete method remains callable after composition';
