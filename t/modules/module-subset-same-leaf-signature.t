use Test;

plan 1;

# Test::Script 0.0.4 has this shape: the unit module's final name is Script,
# and it declares a package-local subset Script used by an exported sub.
module ScriptSignature::Script {
    subset Script of Str where .chars > 0;
    sub accept(Script $script) is export { $script }
}

is ScriptSignature::Script::EXPORT::DEFAULT::<&accept>('ok'), 'ok',
    'a package subset whose leaf matches the package resolves in a sub signature';
