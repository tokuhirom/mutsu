unit module ResSecondUse;

# `use`ing a second distribution BEFORE the module's own `%?RESOURCES` read is
# the shape that broke #8004: any `unit module` that gains a second `use`
# clobbered `package_distributions[<this module's own package>]` with the
# just-loaded dependency's distribution, so a plain named sub (not the
# module's own file-scope mainline) reading `%?RESOURCES` from a `BEGIN`
# resolved against the WRONG distribution's (empty) resource list instead of
# this one's -- exactly `Net::Netmask`/`LLM::Prompts`'s
# `use JSON::Fast; use XDG::BaseDirectory :terms;` shape.
use ResSecondUseDep;

my $greeting;

sub ingest() is export {
    $greeting = %?RESOURCES<greeting.txt>.slurp(:close).trim;
}

BEGIN {
    ingest();
}

sub greeting() is export { $greeting }
