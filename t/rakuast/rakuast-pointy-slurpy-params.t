use Test;

# A pointy block whose one parameter is slurpy (`-> |c`, `-> *@a`) keeps the
# parameter's slurpiness through an EVAL of its RakuAST, as a sub signature
# does; `**@a` stays unflattened.

plan 6;

is EVAL(Q[my &f = -> |c { c.elems }; f(1, 2)].AST), 2, '`-> |c` captures every argument';
is EVAL(Q[my &f = -> |c { 42 }; f()].AST), 42, 'and accepts none';
is EVAL(Q[my &f = -> *@a { @a.elems }; f(1, (2, 3))].AST), 3, '`-> *@a` flattens';
is EVAL(Q[my &f = -> **@a { @a.elems }; f((1, 2), 3)].AST), 2, '`-> **@a` does not';
is EVAL(Q[sub g(**@a) { @a.elems }; g((1, 2), 3)].AST), 2, 'nor does a sub `**@a`';
is EVAL(Q[my &f = -> *%h { %h.elems }; f(:a, :b)].AST), 2, '`-> *%h` takes the named arguments';
