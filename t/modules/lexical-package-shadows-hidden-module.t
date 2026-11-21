use v6;
use lib 't/lib';
use Test;

# A package the running scope declares itself (`my package Name { }`, or one
# nested in the running package) is what `Name` and `Name::Member` mean there,
# even when some other compunit loaded a global package of the same spelling
# that is hidden from this scope. Red's migration compiler EVALs exactly that
# shape (`my package Configuration { }; model Configuration::CliReset ...`)
# while the Configuration distribution is loaded, and died with
# "Could not find symbol 'Configuration::CliReset'" (#18409).

plan 6;

use LexPkgShadowLoader;
use LexPkgShadowCompiler;

is LexPkgShadowLoader.load('class Seen { }; Seen.^name'), 'LexPkgShadowName::Seen',
    'the global package of that name is loaded and used elsewhere';

is compile-it('my package LexPkgShadowName { }; class LexPkgShadowName::C { }; LexPkgShadowName::C.^name'),
    'LexPkgShadowCompiler::LexPkgShadowName::C',
    'a qualified name under a lexical package in an EVAL';

ok compile-it(q:to/CODE/).head.values.map(*.^name).join.ends-with('RMD::LexPkgShadowName::D'),
    do { module RMD {
        my package LexPkgShadowName { }
        class LexPkgShadowName::D { }
        our sub captured { [%('LexPkgShadowName::D' => LexPkgShadowName::D)] } }; RMD::captured() }
    CODE
    'the same name read from a routine of an EVALed module';

# A fresh program whose only route to the global `LexPkgShadowName` is a
# transitive load, so the global name is hidden from its own packages.
sub fresh(Str $code) {
    my $proc = run $*EXECUTABLE, '-It/lib', '-e', $code, :out, :err;
    $proc.out.slurp(:close).trim ~ $proc.err.slurp(:close).trim
}

is fresh(q[module OwnPkg { use LexPkgShadowLoader; my package LexPkgShadowName { };
        class LexPkgShadowName::E { }; print LexPkgShadowName.^name, ' ', LexPkgShadowName::E.^name }]),
    'OwnPkg::LexPkgShadowName OwnPkg::LexPkgShadowName::E',
    'a bare and a qualified name under a package nested in the running package';

is fresh(q[module OwnClass { use LexPkgShadowLoader; my class LexPkgShadowName { };
        print LexPkgShadowName.^name }]),
    'OwnClass::LexPkgShadowName',
    'a bare name bound to a lexical class in the running package';

is fresh(q[use LexPkgShadowLoader; print LexPkgShadowLoader.load('class F { }; F.^name')]),
    'LexPkgShadowName::F',
    'the transitively loaded global package still works through its loader';
