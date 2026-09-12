use v6;
use lib 't/lib';
use Test;

# A `subset` declared `is export` inside a ROLE body is a compile-time
# declaration, not a composition-time side effect: `use`ing the module makes
# the type importable immediately, even though no class has composed the role.
#
# mutsu used to defer the whole role body to composition, so the subset was
# never registered at load time. Two failures followed, both measured across
# the zef corpus by https://github.com/tokuhirom/mutsu/issues/7993:
#
#   * `use Provider :IndRef` imported nothing, so `IndRef` in the consumer
#     decayed to a bareword `Str` and every type check against it failed;
#   * a role that named the imported type in a method signature
#     (`PDF::COS::Tie`'s `multi method deref(IndRef $ind-ref!)`) failed
#     role-composition validation with
#     "Invalid typename 'IndRef' in parameter declaration."
#
# The import order below matters: loading the provider FIRST is what made the
# second failure reproduce, because the consumer's "a body-`use`d module has
# not been loaded yet, defer the check" escape hatch no longer applied.
use RoleBodySubsetProvider :IndRef;
use RoleBodySubsetConsumer;

plan 5;

ok IndRef ~~ Any, 'the exported subset imports as a type, not a bareword Str';
ok ('ind-ref' => [1, 0]) ~~ IndRef, 'its where-clause accepts a matching Pair';
nok ('other' => [1, 0]) ~~ IndRef, 'its where-clause rejects a non-matching Pair';

my $obj;
lives-ok {
    class RoleBodySubsetClass does RoleBodySubsetConsumer { }
    $obj = RoleBodySubsetClass.new;
}, 'a class composes the consumer role without X::Parameter::InvalidType';

is $obj.deref('ind-ref' => 42), 42,
    'the composed method accepts a value matching the imported subset';
