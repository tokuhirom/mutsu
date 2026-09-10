use Test;

plan 4;

# A grammar token used as a negative character-class item may consume more
# than one character.  The exclusion must be checked against the full input,
# not against a one-character scratch string.
grammar Quoted {
    token TOP { '"""' <body>+ '"""' }
    token body { <-delimiter> }
    token delimiter { '"""' }
}

ok Quoted.parse('"""x"""').defined,
    'a negated multi-character token leaves the closing delimiter for its caller';
nok Quoted.parse('"""x"""tail').defined,
    'the grammar still requires the closing delimiter at the end';

grammar Mixed {
    token TOP { <allowed>+ }
    token allowed { <+[a..z] -stop> }
    token stop { 'end' }
}

ok Mixed.parse('abc').defined,
    'a positive class remains available with a negative multi-character token';
nok Mixed.parse('abend').defined,
    'a negative multi-character token excludes its complete sequence';
