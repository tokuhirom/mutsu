use Test;

# `&?ROUTINE` outside any routine of an EVAL'd unit is undeclared, wherever in
# the snippet it sits. The check walks every child through the typed AST
# visitor (ADR-0137), including hash-literal values.

plan 3;

throws-like { EVAL 'my %h = a => &?ROUTINE' }, X::Undeclared::Symbols,
    '&?ROUTINE in a pair value at the mainline';
throws-like { EVAL 'my %h = { a => &?ROUTINE.name }' }, X::Undeclared::Symbols,
    '&?ROUTINE in a hash literal at the mainline';
lives-ok { EVAL 'sub g { my %h = a => &?ROUTINE.name; %h }; g()' },
    '&?ROUTINE in a hash literal inside a sub';
