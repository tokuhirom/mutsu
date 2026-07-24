use v6;
use lib 't/lib';
use Test;

# A subtest body is a block, so everything it declares is rolled back when it
# ends. `use` is lexical too: a module first loaded inside a subtest has its
# declarations rolled back with everything else, so it must also stop counting
# as loaded. Otherwise a later `use` of the same module short-circuits as a
# no-op and its types stay gone for the rest of the file.

plan 6;

subtest 'first load happens inside a subtest' => {
    plan 3;
    use SubtestModuleReuse;
    is SubtestModuleReuse::Thing.new.greet, 'hello', 'class is visible';
    ok SubtestModuleReuse::Thing.new ~~ SubtestModuleReuse::Marker, 'role is visible';
    is subtest-module-reuse-greeting(), 'exported', 'exported sub is visible';
}

subtest 're-using the module in a later subtest works' => {
    plan 3;
    use SubtestModuleReuse;
    is SubtestModuleReuse::Thing.new.greet, 'hello', 'class is visible again';
    ok SubtestModuleReuse::Thing.new ~~ SubtestModuleReuse::Marker, 'role is visible again';
    is subtest-module-reuse-greeting(), 'exported', 'exported sub is visible again';
}

subtest 'a nested subtest can re-use it too' => {
    plan 1;
    subtest 'inner' => {
        plan 1;
        use SubtestModuleReuse;
        is SubtestModuleReuse::Thing.new.greet, 'hello', 'class is visible in nested subtest';
    }
}

# The rollback must not resurrect a module the mainline already loaded: `Test`
# itself was loaded before any subtest ran and must survive.
ok &ok.defined, 'Test is still loaded after the subtests';

# Re-loading has to stay idempotent for declarations the rollback does not
# undo (an enum's variants, a constant), so the mainline can `use` a module the
# subtests already pulled in.
use SubtestModuleReuse;
is SmrGreen.value, 1, 'an exported enum survives a mainline re-use';
is SMR-CONST, 42, 'an exported constant survives a mainline re-use';
