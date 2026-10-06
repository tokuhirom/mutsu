use v6;
use Test;
use lib 't/lib';
use EvalEnumKeyTransitiveFixture;

# A package-less module's top-level enum keys are visible where the module is
# merged (ADR-11136): in the scope that `use`d it, and in the module itself --
# not in a scope that merely loaded something which `use`d it. #11818 made an
# EVAL'd bareword consult enum keys, so it has to honour that gate, exactly as
# a direct bareword and a class name do.
#
# Every expectation below was verified against Rakudo.

plan 4;

is via-key().value, 1, "the module that used the enum sees the key in its own code";

throws-like { EVAL "EvalFixtureSA" }, X::Undeclared::Symbols,
    "a key of a module loaded only transitively is undeclared in the importer's EVAL";
throws-like { EVAL "EvalFixtureSB" }, X::Undeclared::Symbols,
    '... for every key of the enum';

{
    use EvalEnumKeyGlobalFixture;
    is EVAL("EvalFixtureSB").value, 2, 'a scope that uses the module itself sees the key in EVAL';
}
