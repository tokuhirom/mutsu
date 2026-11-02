use v6;
use lib $*PROGRAM.parent(3).add('lib-constant-term-shadow');
use Test;

plan 2;

# The script declares a class named like a constant the module exports.
class G { }

{
    use MG;
    is 3 * G, 'const-G',
        'a constant term in a signature wins over a same-named script class';
}

grammar H { token TOP { a } }
{
    use MG;
    is 3 * G, 'const-G', 'also with a grammar declared in the script';
}
