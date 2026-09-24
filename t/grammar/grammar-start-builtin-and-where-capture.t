use Test;

plan 3;

grammar BuiltinStart { }

ok BuiltinStart.subparse('A', :rule<xdigit>),
    'a grammar can subparse from a built-in regex character class';
ok !BuiltinStart.subparse('G', :rule<xdigit>),
    'a built-in regex character class rejects non-matching input';

grammar CaptureSource {
    token TOP { $<x> = 'x' }
}

class CaptureActions {
    multi method choose($/ where $<x>) { 'named capture' }
    multi method choose($/ where $<y>) { 'other capture' }
}

is CaptureActions.new.choose(CaptureSource.parse('x')),
    'named capture',
    'dispatch-time where predicates can inspect captures from a $/ argument';
