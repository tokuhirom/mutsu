use Test;

plan 5;

my \Later = EVAL q[class Later { }; Later];
is Later.^name, 'Later', 'an EVAL class shadows the enclosing sigilless term';

my \Lexical = EVAL q[my class Lexical { }; Lexical];
is Lexical.^name, 'Lexical', 'an EVAL lexical class shadows the enclosing term';

my \Outside = 9;
is EVAL('class Outside { }; Outside.^name'), 'Outside',
    'the EVAL class is read inside its own unit';
is Outside, 9, 'the enclosing term remains bound after EVAL';

my \Different = EVAL q[class Other { }; Other];
is Different.^name, 'Other', 'a different enclosing term name still binds the result';
