use Test;

plan 3;

# Braces in quoted strings inside a regex code assertion are code, not
# delimiters for the assertion body.
grammar G {
    regex TOP { (.+) <?{ '}' eq '}' }> }
}

ok G.parse('x{').defined,
    'a quoted closing brace does not terminate a grammar code assertion';
ok ('ab' ~~ / . <?{ '}' eq '}' }> /).defined,
    'a quoted closing brace does not terminate a standalone code assertion';
ok ('ab' ~~ / . <?{ '{' eq '{' }> /).defined,
    'a quoted opening brace does not affect code assertion balancing';
