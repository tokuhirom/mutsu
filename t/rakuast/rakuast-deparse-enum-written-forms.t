use Test;
use experimental :rakuast;

plan 8;

# An enum body keeps how it was written: colonpair members and the raw
# spacing of a `<< >>` word quote (#14845).
sub dep($src) { $src.AST.DEPARSE.trim }

is dep('enum V3 (:x(1), :y(2));'), 'enum V3 (:x(1), :y(2))', 'colonpair members';
is dep('enum V4 (a => 1, b => 2);'), 'enum V4 (a => 1, b => 2)', 'fat-arrow members are unchanged';
is dep('my enum C3 << a b >>;'), 'my enum C3 << a b >>', 'spaces inside << >> are kept';
is dep('my enum C2 <<rgb rgba>>;'), 'my enum C2 <<rgb rgba>>', 'no spaces stay none';
is dep('my enum C4 «a b»;'), 'my enum C4 <<a b>>', 'guillemets print as <<>>';

is EVAL('enum E1 (:p(3), :q(4)); E1::q.value'.AST), 4, 'EVAL of colonpair members';
is EVAL('enum E2 << a b >>; E2::b.value'.AST), 1, 'EVAL of a spaced << >> body';
is EVAL('enum E3 << :k(7) m >>; E3::m.value'.AST), 8, 'EVAL of a quote with a valued atom';
