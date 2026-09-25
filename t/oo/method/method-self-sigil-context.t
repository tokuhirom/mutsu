use Test;

plan 9;

# `@.meth(args)` / `@.meth: args` (and `%.meth ...`) call the method on self
# and put the result in the sigil's context: `@(self.meth(args))`. (#9323)

class A {
    method protect(&c) { c() }
    method pair($x)    { a => $x }
    method two($x)     { $x, 2 }
    method one($x)     { $x }
    method count(*@x)  { @x.elems }
    method seven()     { 7 }
    has @.attr = 1, 2;

    method colon-block  { @.protect: { 42 } }
    method paren-args   { @.two(1) }
    method colon-args   { @.two: 1 }
    method hash-colon   { %.pair: 1 }
    method hash-paren   { %.pair(1) }
    method list-of-one  { @.one(5) }
    method colon-list   { @.count: 1, 2, 3 }
    method colon-empty  { @.seven: }
    method accessor     { @.attr }
}

my $a = A.new;
is-deeply $a.colon-block, (42,), '@.meth: { block } (PDF::Font::Loader shape)';
is-deeply $a.paren-args, (1, 2), '@.meth(args) passes its arguments';
is-deeply $a.colon-args, (1, 2), '@.meth: args';
is-deeply $a.hash-colon, {a => 1}, '%.meth: args is hash-contextualized';
is-deeply $a.hash-paren, {a => 1}, '%.meth(args)';
is $a.list-of-one.raku, '(5,)', 'a single value comes back as a one-element list';
is $a.colon-list, '3', 'the whole comma list is the colon arglist';
is-deeply $a.colon-empty, (7,), 'an empty colon arglist is a zero-argument call';
is-deeply $a.accessor, [1, 2], 'the bare @.attr accessor is unchanged';
