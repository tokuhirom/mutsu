use Test;

plan 6;

# A sigilless `my \Name` at an EVAL'd unit's top level belongs to that unit,
# whatever its first letter (#14227).
EVAL q[my \Zork := 42; 1];
is (try EVAL 'Zork') // 'gone', 'gone', 'my \Name := does not outlive the EVAL';
EVAL q[my \Zork2 = 42; 1];
is (try EVAL 'Zork2') // 'gone', 'gone', 'my \Name = does not outlive the EVAL';
EVAL q[my \zork := 42; 1];
is (try EVAL 'zork') // 'gone', 'gone', 'lowercase my \name does not outlive the EVAL';
EVAL q[constant KK = 3; 1];
is (try EVAL 'KK') // 'gone', '3', 'a constant is our-scoped and stays';

# A caller's own binding of the same name is given back.
my \Own = 7;
EVAL q[my \Own = 99; 1];
is Own, 7, "caller's \\Own is restored";
is EVAL(q[my \Inner = 5; Inner]), 5, 'the declaration is visible inside its own EVAL';
