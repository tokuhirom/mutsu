use v6;
use lib 't/lib/ResSecondUse/lib', 't/lib/ResSecondUseDep/lib';
use Test;

plan 1;

# #8004: `%?RESOURCES` inside a `unit module` that `use`s a second
# distribution before reading its own resources. `package_distributions`
# recorded the distribution both under the module's own name AND under
# `self.current_package()` read at the moment each dependency load started --
# but by the time the SECOND `use` runs, `current_package()` already reflects
# the ENCLOSING module's own name (its `unit module` statement having already
# set it), so loading the dependency stamped the enclosing module's own
# `package_distributions` entry with the dependency's distribution instead.
# A plain named sub (not the module's file-scope mainline, so the def_file-
# based lookup does not apply) reading `%?RESOURCES` from a `BEGIN` then saw
# the WRONG distribution's (empty) resource list and `%?RESOURCES<name>` came
# back `Any`, blowing up `.slurp` — the `Net::Netmask`/`LLM::Prompts`
# `use JSON::Fast; use XDG::BaseDirectory :terms;` shape.
use ResSecondUse;

is greeting(), 'hello from ResSecondUse',
    '%?RESOURCES resolves correctly even after a second `use` inside the same unit module';
