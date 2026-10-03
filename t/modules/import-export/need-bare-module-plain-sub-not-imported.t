use Test;

# A `need` loads a module without importing anything: a package-less
# module's exported plain `sub` must stay out of the loading scope (#11080),
# while its EXPORT stash still holds it and a later `use` still imports it.
# Checked against Rakudo.

plan 5;

use lib 't/lib';
need NeedPlainExport;

throws-like { EVAL 'npe-exported()' }, X::Undeclared::Symbols,
    'need does not import an exported plain sub';
throws-like { EVAL 'npe-helper()' }, X::Undeclared::Symbols,
    'nor a private one';
nok MY::<&npe-exported>:exists, 'no &npe-exported binding in the loading scope';

is EVAL('use NeedPlainExport; npe-exported()'), 'exported',
    'a later use of the needed module imports the sub';
is EVAL('use NeedPlainExport; npe-calls-helper()'), 'helper',
    "and the imported sub still reaches the module's private helper";
