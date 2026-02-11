use Test;
plan 10;

# orelse (defined-or at low precedence)
is (1 orelse 2), 1, 'orelse: defined value returns left';
is (Nil orelse 2), 2, 'orelse: Nil falls through to right';
is (Nil orelse Nil orelse 3), 3, 'orelse: chained';
is (0 orelse 2), 0, 'orelse: 0 is defined, returns 0';
is (1 orelse 0), 1, 'orelse: definedness not truthness';

# andthen (defined-and at low precedence)
is (1 andthen 2), 2, 'andthen: defined value passes through';
is (Nil andthen 2), Nil, 'andthen: Nil short-circuits';
is (1 andthen 2 andthen 3), 3, 'andthen: chained all defined';
is (1 andthen Nil andthen 3), Nil, 'andthen: chained middle Nil';
is (0 andthen 2), 2, 'andthen: 0 is defined, passes through';
