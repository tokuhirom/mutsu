use v6;
use Test;

# A class or role declared with `:ver`/`:auth`/`:api` adverbs keeps its body's
# `my sub`s and `my` variables in scope for its methods and attribute
# defaults. The adverbs used to wrap the declaration in a lexical block that
# ended the class body's scope (File::Stat's `class File::Stat:auth<..>:ver<..>`).

plan 5;

class C:auth<zef:someone>:ver<1.0.3> {
    my sub plus-one($x) { $x + 1 }
    my $base = 10;
    has &!op = &plus-one;
    method run($x) { &!op($x) }
    method base { $base }
    method direct { plus-one(41) }
}
is C.new.run(1), 2, 'an attribute default can take a `my sub` of the body';
is C.new.base, 10, 'a method sees a body `my` variable';
is C.new.direct, 42, 'a method calls a body `my sub`';
is C.^ver, v1.0.3, 'the version adverb still applies';

role R:ver<2> {
    my sub three { 3 }
    method q { three() }
}
is (class :: does R {}).new.q, 3, 'a role body `my sub` stays visible to its methods';
