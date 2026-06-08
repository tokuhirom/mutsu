use Test;

plan 11;

# Routine redeclaration in the same scope.
throws-like 'sub a { }; sub a { }', X::Redeclaration,
    symbol => 'a', what => 'routine',
    'two non-multi subs of the same name redeclare';
throws-like 'sub a { }; multi sub a { }', X::Redeclaration,
    symbol => 'a', what => 'routine',
    'non-multi sub then multi sub redeclares';
throws-like 'multi sub a { }; sub a { }', X::Redeclaration,
    symbol => 'a', what => 'routine',
    'multi sub then non-multi sub redeclares';
lives-ok { EVAL 'multi sub a(Int) { }; multi sub a(Str) { }' },
    'two multi subs of the same name are allowed';

# Cross-kind type redeclaration (class / role / subset share one namespace).
throws-like 'my class B { }; my subset B of Any;', X::Redeclaration,
    symbol => 'B', 'class then subset of the same name redeclares';
throws-like 'my class F { }; role F { }', X::Redeclaration,
    symbol => 'F', 'class then role of the same name redeclares';
throws-like 'my class A { }; my class A { }', X::Redeclaration,
    symbol => 'A', 'two classes of the same name redeclare';

# Return type declared twice.
throws-like 'sub f(--> List) returns Str { }', X::Redeclaration,
    'returns plus --> redeclares the return type';
throws-like 'my Int sub f(--> Str) { }', X::Redeclaration,
    'my Type prefix plus --> redeclares the return type';

# Nested-type redeclaration via augment.
throws-like 'class AAAA { class B {} }; use MONKEY; augment class AAAA { class B { } }',
    X::Redeclaration, symbol => 'B',
    'augment redeclaring a nested class throws';
throws-like 'class AAAAA { class B::C {} }; use MONKEY; augment class AAAAA { class B::C { } }',
    X::Redeclaration, symbol => 'B::C',
    'augment redeclaring a nested qualified class throws';
