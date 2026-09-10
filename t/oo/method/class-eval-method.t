use Test;

plan 7;

# EVAL inside a class body can add methods to the class
{
    class EvClass {
        EVAL('method x { "OH HAI" }')
    };
    is EvClass.x, "OH HAI", "EVAL adds method to enclosing class";
}

# self is not assignable
throws-like 'class WS { method f { self = 5 } }; WS.new.f',
    X::Assignment::RO, 'self is not writable in a method';

# class:D should throw X::Syntax::Type::Adverb
throws-like 'class Foo_D:D {}', X::Syntax::Type::Adverb,
    'cannot declare class with :D adverb';

# class:U should also throw
throws-like 'class Foo_U:U {}', X::Syntax::Type::Adverb,
    'cannot declare class with :U adverb';

# class:no_such should throw
throws-like 'class Foo_NS:no_such {}', X::Syntax::Type::Adverb,
    'cannot declare class with unknown adverb';

# class:auth and :ver are allowed
eval-lives-ok 'class Adv:auth<me>:ver<1.0> { }',
    'class with :auth and :ver adverbs is allowed';

# class:api is allowed
eval-lives-ok 'class AdvApi:api<2> { }',
    'class with :api adverb is allowed';
