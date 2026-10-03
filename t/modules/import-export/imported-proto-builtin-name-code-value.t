use Test;

use lib 't/lib';
use ImportedProtoBuiltinName;

plan 8;

# An imported proto named like a core routine, called through its code
# value, dispatches to the imported candidates rather than the core routine.
is reverse('Foo'), 'ooF', 'direct call reaches the imported candidate';
is 'Bar'.&reverse, 'raB', '.&reverse reaches the imported candidate';
is &reverse('Bar'), 'raB', '&reverse(...) reaches the imported candidate';
with 'Bar' { is .&reverse, 'raB', '.&reverse on the topic' }
with 1, 2, 3 { is .&reverse, '3 2 1', 'the List candidate through .&reverse' }

my &r = &reverse;
is r('abc'), 'cba', 'a code value stored in another variable';

is 'x'.&sort, 'user-sort:x', '.&sort reaches the imported candidate';
is &sort.('y'), 'user-sort:y', '&sort.(...) reaches the imported candidate';
