use Test;
use MONKEY-SEE-NO-EVAL;

# A built-in type that mutsu declares from a prelude is visible to an EVAL'd
# unit that is the only place naming it: the program text built the name at
# run time, so nothing but the EVAL could ask for the prelude.

plan 4;

is EVAL('Operator' ~ 'Properties.^name'), 'OperatorProperties',
    'OperatorProperties named only inside EVAL';
is EVAL('Operator' ~ 'Properties.new(:precedence("t=")).raku'),
    'OperatorProperties.new: :precedence("t=")',
    'its instances work';
is EVAL('my \e = Numeric' ~ 'Enumeration; e.^name'), 'NumericEnumeration',
    'NumericEnumeration named only inside EVAL';
throws-like { EVAL('NoSuch' ~ 'TypeHere.new') }, X::Undeclared::Symbols,
    'an unknown type is still undeclared';
