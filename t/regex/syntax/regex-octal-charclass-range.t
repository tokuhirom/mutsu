use Test;

plan 3;

grammar Latin1 {
    token TOP { <[\o241..\o377]> }
}

ok Latin1.parse("¡"), 'an unbracketed octal range includes its lower bound';
ok Latin1.parse("ÿ"), 'an unbracketed octal range includes its upper bound';
nok Latin1.parse("\x95"), 'an unbracketed octal range excludes characters below its lower bound';
