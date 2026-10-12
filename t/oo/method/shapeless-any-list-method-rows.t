use Test;

# ADR-11276 §9.80: Any's list methods (end, flat, sort, reverse, unique,
# repeated) reach every receiver through the method table, without the
# name-matching cascade arms that used to answer them.
#
# - A closed shape reaches an audited subset of Any's rows.
# - A receiver with no shape (a code object, a promise, a type object with no
#   shape, an enum value, an instance) reaches them once the cascades decline.
# - A type object reaches them: Any's rows list a type object as itself.

plan 6;

subtest 'a receiver with no shape', {
    plan 6;
    sub f { 1 }
    class P { }
    enum E <x y>;
    is-deeply &f.end, 0, 'a Sub';
    is-deeply Promise.new.flat.elems, 1, 'a Promise';
    is-deeply P.new.end, 0, 'an instance';
    is-deeply P.new.unique.elems, 1, 'unique of an instance';
    is-deeply x.flat.List, (x,), 'an enum value';
    is-deeply (gather { take 3; take 1 }).end, 1, 'a gather Seq';
}

subtest 'a type object lists as itself', {
    plan 5;
    is-deeply Any.end, 0, 'Any.end';
    is-deeply Any.flat.List, (Any,), 'Any.flat';
    is-deeply Str.sort.List, (Str,), 'Str.sort';
    is-deeply Int.end, 0, 'Int.end';
    is-deeply Str.unique.List, (Str,), 'Str.unique';
}

subtest 'a closed shape', {
    plan 5;
    is-deeply (a => 1).reverse.List, (a => 1,), 'Pair.reverse';
    is-deeply set(<a>).end, 0, 'Set.end';
    is-deeply Buf.new(1, 2).end, 1, 'Buf.end';
    is-deeply "a".IO.repeated.List, (), 'IO::Path.repeated';
    is-deeply (v1.2).flat.List, (v1.2,), 'Version.flat';
}

subtest 'the quant hashes read as their pairs for unique', {
    plan 1;
    is-deeply bag(<a a>).unique.List, (:a(2),), 'Bag.unique';
}

subtest 'a scalar and a collection', {
    plan 4;
    is-deeply 42.end, 0, 'Int.end';
    is-deeply "x".reverse.List, ("x",), 'Str.reverse';
    is-deeply [3, 1, 2].sort.List, (1, 2, 3), 'Array.sort';
    is-deeply (1, 1, 2).repeated.List, (1,), 'List.repeated';
}

dies-ok { Failure.new("f").end }, 'a Failure still explodes';
