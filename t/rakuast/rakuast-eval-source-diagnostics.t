use Test;
use MONKEY-SEE-NO-EVAL;
use lib 't/lib';
use EvalImportedConstantClass;

# In frontend mode the source checks precede conversion. A source error must
# retain its typed exception even when the invalid construct cannot convert.
for 'say missing-control-term;',
    'my $value = missing-control-term; $value',
    'if 1 -> $value { missing-control-term }',
    'unless 0 -> $value { missing-control-term }',
    'with Any { 1 } orwith missing-control-term() -> $value { $value }',
    'while 0 -> $value { missing-control-term }',
    'sub local-routine { missing-control-term }; local-routine()'
    -> $source {
    throws-like { EVAL($source) }, X::Undeclared::Symbols,
        'an undeclared source term retains its CHECK-time exception';
}

throws-like { EVAL('my $value = UnknownControlType.new; $value') },
    X::Undeclared::Symbols, 'an undeclared qualified call target retains its exception';
throws-like { EVAL('sub unknown-param(UnknownControlType $value) { $value }') },
    X::Parameter::InvalidType, 'an invalid parameter type keeps its own diagnostic';

my $ran = 0;
throws-like { EVAL('$ran = 1; missing-control-term') }, X::Undeclared::Symbols,
    'source checking rejects the whole unit before running its body';
is $ran, 0, 'a rejected EVAL does not run preceding ordinary statements';

is EVAL('sub local-routine { 7 }; local-routine'), 7,
    'a declared bare routine still converts and executes';
is EVAL('my $x = 4; if $x -> $value { $value + 3 }'), 7,
    'a valid control signature still executes after checking';
is EVAL('do with Any { 0 } orwith (3, 4) -> ($a, $b) { $a + $b }'), 7,
    'a valid orwith destructure still crosses the frontend boundary';

is EVAL('use EvalExportHookTerm <U>; U.k'), 1,
    'a dynamic export remains valid after source checking';
throws-like { EVAL('use EvalExportHookTerm <U>; NoSuchTerm') },
    X::Undeclared::Symbols, 'resolved names are checked again after lowering';

is EVAL('EVAL_IMPORTED_CLASS_CONST'), 42,
    'source-check module probes preserve imported caller constant names';

done-testing;
