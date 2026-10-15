use Test;

# A closure reads a free `@`/`%` lexical through its captured binding (an
# upvalue, ADR-0039 §4.2, #9895), so it sees the container of the scope it was
# written in, including changes made to it after the closure was created, and
# never a same-named container of the frame that calls it.
plan 8;

{
    my @a = 1, 2;
    my %h = a => 1;
    my &f = -> { @a.elems ~ ':' ~ %h.keys.sort.join(',') };
    @a.push: 3;
    %h<b> = 2;
    is f(), '3:a,b', 'later mutations of the captured containers are seen';
}

{
    my @a = 1, 2;
    sub caller-of(&c) { my @a = 9, 9, 9, 9; c() }
    is caller-of(-> { @a.elems }), 2, 'a caller\'s same-named @a is not read';
    my %h = x => 1;
    sub caller-of-h(&c) { my %h = y => 1, z => 2; c() }
    is caller-of-h(-> { %h.keys.join }), 'x', 'nor a caller\'s same-named %h';
}

{
    my @made;
    for 1, 2 -> $i {
        my @per = $i xx $i;
        @made.push: -> { @per.elems };
    }
    is @made.map({ $_() }).join(','), '1,2', 'each loop iteration\'s container is its own';
}

{
    my @a = 1;
    my &outer = -> { -> { @a.elems } };
    @a.push: 2;
    is outer()(), 2, 'a nested closure reaches the container too';
}

{
    sub with-param(@p) { -> { @p.join } }
    is with-param([1, 2])(), '12', 'a captured array parameter';
    sub with-hash(%p) { -> { %p.keys.sort.join } }
    is with-hash({ :a, :b })(), 'ab', 'a captured hash parameter';
}

{
    my @a = 1, 2;
    my &g = -> { @a.push: 9; @a.elems };
    is g(), 3, 'a closure that mutates the container still writes it';
}
