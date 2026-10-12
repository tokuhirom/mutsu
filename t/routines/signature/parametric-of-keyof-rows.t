use Test;

# ADR-11276 §9.86: `of` and `keyof` are rows of the classes and roles Rakudo
# declares them on (`Hash`, `Array`, `Blob`, `Positional`, `Associative`,
# `Callable`, `Baggy`, `Setty`, `Mixy`, `QuantHash`). A type object with no
# dispatch shape (a role, a code type, a parameterized type) reaches them by
# its base type's MRO; the parameters are read from its name.

plan 10;

subtest 'unparameterized type objects', {
    plan 20;
    is Hash.of.raku, 'Mu', 'Hash.of';
    is Hash.keyof.raku, 'Str(Any)', 'Hash.keyof';
    is Array.of.raku, 'Mu', 'Array.of';
    is List.of.raku, 'Mu', 'List.of (Positional)';
    is Range.of.raku, 'Mu', 'Range.of (Positional)';
    is Map.of.raku, 'Mu', 'Map.of (Associative)';
    is Map.keyof.raku, 'Str(Any)', 'Map.keyof (Associative)';
    is Pair.of.raku, 'Mu', 'Pair.of (Associative)';
    is Pair.keyof.raku, 'Str(Any)', 'Pair.keyof (Associative)';
    is Positional.of.raku, 'Mu', 'Positional.of';
    is Associative.keyof.raku, 'Str(Any)', 'Associative.keyof';
    is Callable.of.raku, 'Mu', 'Callable.of';
    is Sub.of.raku, 'Mu', 'Sub.of';
    is Regex.of.raku, 'Mu', 'Regex.of';
    is Stash.keyof.raku, 'Str(Any)', 'Stash.keyof (Hash)';
    is Bag.of.raku, 'UInt', 'Bag.of (Baggy)';
    is Set.of.raku, 'Bool', 'Set.of (Setty)';
    is MixHash.of.raku, 'Real', 'MixHash.of (Mixy before Baggy)';
    is Baggy.of.raku, 'UInt', 'Baggy.of';
    is QuantHash.keyof.raku, 'Mu', 'QuantHash.keyof';
}

subtest 'parameterized type objects', {
    plan 12;
    is Hash[Int,Str].of.raku, 'Int', 'Hash[Int,Str].of';
    is Hash[Int,Str].keyof.raku, 'Str', 'Hash[Int,Str].keyof';
    is Hash[Int].keyof.raku, 'Str(Any)', 'Hash[Int].keyof';
    is Hash[Hash[Int,Str],Int].of.raku, 'Hash[Int,Str]', 'a nested parameter is not split';
    is Array[Int].of.raku, 'Int', 'Array[Int].of';
    is Positional[Int].of.raku, 'Int', 'Positional[Int].of';
    is Associative[Int,Str].keyof.raku, 'Str', 'Associative[Int,Str].keyof';
    is Callable[Int].of.raku, 'Int', 'Callable[Int].of';
    is Bag[Int].of.raku, 'UInt', 'Bag[Int].of';
    is Bag[Int].keyof.raku, 'Int', 'Bag[Int].keyof';
    is Set[Str].keyof.raku, 'Str', 'Set[Str].keyof';
    is Buf[uint32].of.raku, 'uint32', 'Buf[uint32].of';
}

subtest 'instances', {
    plan 10;
    my %h; my Int %hi; my %hk{Int};
    is %h.of.raku ~ ' ' ~ %h.keyof.raku, 'Mu Str(Any)', 'plain hash';
    is %hi.of.raku, 'Int', 'typed hash';
    is %hk.keyof.raku, 'Int', 'object hash';
    my Int @a;
    is @a.of.raku, 'Int', 'typed array';
    my @sh[2];
    is @sh.of.raku, 'Mu', 'shaped array';
    is (1, 2).of.raku, 'Mu', 'List instance';
    is (1..2).of.raku, 'Mu', 'Range instance';
    is (a => 1).keyof.raku, 'Str(Any)', 'Pair instance';
    is Map.new((a => 1)).of.raku, 'Mu', 'Map instance';
    is buf16.new(1).of.raku, 'uint16', 'buf16 instance';
}

subtest 'quant hash instances', {
    plan 5;
    is bag(<a>).of.raku, 'UInt', 'bag';
    is Bag[Int].new(1).keyof.raku, 'Int', 'Bag[Int].new';
    is SetHash[Int].new(1).keyof.raku, 'Int', 'SetHash[Int].new';
    my %bh is BagHash[Int];
    is %bh.keyof.raku, 'Int', 'is BagHash[Int] keeps its parameter';
    is %bh.of.raku, 'UInt', 'is BagHash[Int] values are weights';
}

subtest 'type objects bound to container parameters', {
    plan 3;
    my class Dog {}
    sub g(@x) { @x.of.raku }
    is g(Positional[Dog]), 'Dog', 'Positional[Dog] bound to @x';
    sub h(%x) { %x.of.raku ~ ' ' ~ %x.keyof.raku }
    is h(Hash[Int,Str]), 'Int Str', 'Hash[Int,Str] bound to %x';
    is h(Associative[Int]), 'Int Str(Any)', 'Associative[Int] bound to %x';
}

subtest 'no candidate', {
    plan 4;
    throws-like { Seq.of }, X::Method::NotFound, 'Seq has no of';
    throws-like { Int.of }, X::Method::NotFound, 'Int has no of';
    throws-like { Array.keyof }, X::Method::NotFound, 'Array has no keyof';
    throws-like { Mu.of }, X::Method::NotFound, 'Mu has no of';
}

subtest 'user types keep their own answers', {
    plan 2;
    my class A does Positional[Int] {}
    is A.of.raku, 'Int', 'a class composing Positional[Int]';
    my role R { method of { 'custom' } }
    my %m = a => 1;
    is (%m but R).of, 'custom', 'a mixin declaring of';
}

subtest 'code objects', {
    plan 2;
    is (sub (--> Int) {}).of.raku, 'Int', 'a sub with a return type';
    is (-> {}).of.raku, 'Mu', 'a block';
}

subtest 'itemized containers', {
    plan 2;
    my Int %hi;
    my $ref = %hi;
    is $ref.of.raku ~ ' ' ~ $ref.keyof.raku, 'Int Str(Any)', 'itemized typed hash';
    my $ia = [1];
    is $ia.of.raku, 'Mu', 'itemized array';
}

subtest 'a class trait names the element type', {
    plan 2;
    my class A is Array[Str] { }
    my @a is A = <a b c>;
    is-deeply @a.of, Str, 'my @a is A, A is Array[Str]';
    my @b is A = <d>;
    my $capture = { @b[0] };
    is-deeply @b.of, Str, 'also when a closure captures the variable';
}
