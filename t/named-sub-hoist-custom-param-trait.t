use Test;
use lib 't/lib';
use QueryTraitFixture;

# A named top-level `sub` is hoisted (registered early, so its name is
# callable before its textual position). The hoist pre-pass used to run the
# parameter-trait validation (`check_param_custom_traits`) too, which fires
# before the preceding `use`'s custom `trait_mod:<is>` (Cro::HTTP::Router's
# `is query` et al.) has had a chance to register — turning a legal custom
# parameter trait into a hard "unknown trait" error before the script's first
# statement even runs. The in-sequence (non-hoisted) registration always ran
# after every earlier `use`, so the fix is to skip the check on the hoist
# pass and let that second registration validate it for real.

plan 2;

sub search(:$term is query) { $term }

is search(term => 'raku'), 'raku', 'a hoisted sub with a use-provided custom param trait still runs';
ok &search.signature.params[0] ~~ Query, 'the trait role is composed onto the parameter';
