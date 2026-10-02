use v6;
use lib 't/lib';
use Test;

# ADR-0081 §6 "open checks": the less common paths through a unit module's
# compunit-scoped import aliases (tokuhirom/mutsu#9925). The main repro lives
# in module-import-alias-scope.t; each path here must see the module's own
# imports and must not leak them into the caller.

plan 16;

use Issue9925::Mid;

# True when the name resolves from the caller (file) scope at all; a type
# object counts, which `.defined` would miss. A miss is a Failure, which
# `.so` both reports as False and marks handled.
sub leaked(Str $name) {
    my \found = ::($name);
    found ~~ Failure ?? found.so !! True
}

# The block-scoped `use Issue9925::Vars` further down is preloaded at the head
# of this unit, and that preload leaves the module's `our` names bound in the
# file scope (#11009), so the two variable checks below are TODO until then.

# --- `require` from inside a method --------------------------------------
is I9925Loader.new.load, 'imported/hi/late',
    'a method that requires a module still sees its unit imports';
is I9925Loader.new.load, 'imported/hi/late',
    'a second require from the method (module already loaded) behaves the same';
todo 'a preloaded module leaves its our names bound (#11009)';
nok leaked('$i9925-value'), 'the unit import does not leak to the caller after require';

# --- EVAL nested in an imported routine -----------------------------------
is i9925-eval(), 'imported/hi/<e>',
    'an EVAL inside an imported routine sees the routine unit imports';
nok leaked('I9925Class'), 'the imported class does not leak to the caller after EVAL';
nok leaked('&i9925-tag'), 'the imported sub does not leak to the caller after EVAL';

# --- A closure handed to a native callback --------------------------------
my ($sorted, $tagged) = i9925-native-sorted(3, 1, 4, 2);
is-deeply $sorted, (4, 3, 2, 1),
    'a native callback closure reads the unit imported variable';
ok $tagged, 'a native callback closure calls the unit imported sub';
todo 'a preloaded module leaves its our names bound (#11009)';
nok leaked('$i9925-direction'), 'the imported variable does not leak after the callback';

# --- Two modules importing the same short type name -----------------------
use Issue9925::UserA;
use Issue9925::UserB;
is user-a-who(), 'A/Issue9925::ThingA::Thing',
    'the first module resolves the short name to its own import';
is user-b-who(), 'B/Issue9925::ThingB::Thing',
    'the second module resolves the same short name to its own import';
is user-a-who(), 'A/Issue9925::ThingA::Thing',
    'the first module still sees its own import after the second loaded';
nok leaked('Thing'), 'neither module leaks the short type name to the caller';

# --- A block-scoped `use`, then a repeated one after the module is loaded --
{
    use Issue9925::Vars;
    is $i9925-value, 'imported', 'a block-scoped use imports into the block';
}
{
    use Issue9925::Vars;
    is i9925-tag($i9925-value), '<imported>',
        'a repeated block-scoped use of the loaded module imports again';
}
nok leaked('$i9925-value'), 'neither block-scoped use leaks to the enclosing scope';
