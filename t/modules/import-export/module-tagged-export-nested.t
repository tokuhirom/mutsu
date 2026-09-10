use v6;
use Test;

plan 4;

# A module that imports a tag-gated sub (`use Other :internals`) must be able to
# call that sub from its own methods, even when the outer program `use`s the
# module without requesting the tag. Previously mutsu hoisted every unit-module
# export into a single GLOBAL bag and the outer `use` tag-filter stripped the
# transitively-imported sub, breaking the inner module. Surfaced while driving
# the real zef CLI: its git fetcher does `use Zef::Utils::URI :internals` and
# calls `uri(...)` from its own methods.

use lib $?FILE.IO.parent.add('lib-tagged-export').Str;

use UsesInternals;
my $o = UsesInternals.new;
is $o.run("X"), "internal-X",
    'a single tagged `use :internals` stays visible to the module methods';

use UsesInternalsDouble;
my $d = UsesInternalsDouble.new;
is $d.run("Y"), "internal-Y",
    'a `use; use :internals` re-import restores the tag-gated sub for module methods';
is $d.plain(), "always",
    'the default export from the same module also resolves';

# A directly-requested tagged import DOES expose the sub.
{
    use TagExp :internals;
    is internal("Z"), "internal-Z",
        'a direct tagged import exposes the sub in the requesting scope';
}

# NOTE: leak-prevention (the tag-gated sub staying hidden from the OUTER program
# when only the module class was imported) is a separate, broader limitation of
# mutsu's flat GLOBAL function namespace and is not asserted here. See
# t/module-tagged-export-default.t for the case that IS guaranteed: a plain
# default `use` never exposes a :tag-only export.
