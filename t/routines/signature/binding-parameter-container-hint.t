use Test;

# A container binding failure carries rakudo's beginner hint, and the
# explanation (`expected ... but got ...` plus the hint) is word-wrapped at 72
# columns the way rakudo's `naive-word-wrapper` does (#10815).

plan 12;

sub msg(&code) { code(); 'lived'; CATCH { default { return .message } } }

is msg({ sub h(%h) {}; h([1]) }),
    "Type check failed in binding to parameter '%h'; expected Associative but got Array. You have to pass an explicitly\ntyped hash, not one that just might happen to contain elements of the\ncorrect type.",
    'an untyped % parameter given an Array';

is msg({ sub t(Int @a) {}; t(["b"]) }),
    "Type check failed in binding to parameter '@a'; expected Positional[Int] but got Array ([\"b\"]). You have to pass an\nexplicitly typed array, not one that just might happen to contain\nelements of the correct type.",
    'a typed @ parameter given an untyped Array';

is msg({ sub u(Int %h) {}; u({a => "b"}) }),
    "Type check failed in binding to parameter '%h'; expected Associative[Int] but got Hash. You have to pass an explicitly\ntyped hash, not one that just might happen to contain elements of the\ncorrect type.",
    'a typed % parameter given an untyped Hash';

is msg({ sub f(Array @a) {}; f([[1],]) }),
    "Type check failed in binding to parameter '@a'; expected Positional[Array] but got Array ([[1],]). Did you mean to\nexpect an array of Arrays?",
    'an array-of-Arrays parameter suggests that was meant';

is msg({ sub f(Hash $x) {}; f((1, 2)) }),
    "Type check failed in binding to parameter '\$x'; expected Hash but got List. You have to pass an explicitly typed hash,\nnot one that just might happen to contain elements of the correct type.",
    'an Associative type on a $ parameter names the argument by its type alone';

is msg({ sub f(List $x) {}; f({a => 1}) }),
    "Type check failed in binding to parameter '\$x'; expected List but got Hash (\{:a(1)}). You have to pass an explicitly\ntyped array, not one that just might happen to contain elements of the\ncorrect type.",
    'a Positional type on a $ parameter';

is msg({ sub h(Int %a) {}; my $v = (a => 1); h($v) }),
    "Type check failed in binding to parameter '%a'; expected Associative[Int] but got Pair. You have to pass an explicitly\ntyped hash, not one that just might happen to contain elements of the\ncorrect type.",
    'a Pair has an untyped .of';

is msg({ sub h(Int @a) {}; my $v = Array; h($v) }),
    "Type check failed in binding to parameter '@a'; expected Positional[Int] but got Array (Array). You have to pass an\nexplicitly typed array, not one that just might happen to contain\nelements of the correct type.",
    'a bare Array type object';

# No hint when the argument is not an untyped container.
is msg({ sub h(Int @a) {}; my $v = 5; h($v) }),
    "Type check failed in binding to parameter '@a'; expected Positional[Int] but got Int (5)",
    'a non-container gets no hint';

is msg({ sub h(Int @a) {}; h(set(1)) }),
    "Type check failed in binding to parameter '@a'; expected Positional[Int] but got Set (Set.new(1))",
    'a Set (whose .of is Bool) gets no hint';

is msg({ sub h(Int $a) {}; h([1]) }),
    "Type check failed in binding to parameter '\$a'; expected Int but got Array ([1])",
    'a non-container expectation gets no hint';

is msg({ sub h(Int @aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) {}; h([1 xx 60]) }),
    "Type check failed in binding to parameter '@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'; expected Positional[Int] but got Array ([1, 1, 1, 1, 1, 1, 1...). You\nhave to pass an explicitly typed array, not one that just might happen\nto contain elements of the correct type.",
    'only the explanation is wrapped, not the lead-in';
