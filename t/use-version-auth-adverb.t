use lib 't/lib';
use Test;

# `use Module:ver<...>:auth<...>` — the version/auth selectors must be
# consumed as distribution selectors, NOT treated as import tags (which
# previously failed with `no such tag 'ver' declared`).
use VersionedMod:ver<1.2.3+>:auth<zef:mutsu-test>;

plan 4;

# The unit module declared its own :ver<>/:auth<> adverbs and still loads.
is versioned-greet(), "hello from versioned",
    "use Module:ver<>:auth<> imports exported sub";

# :ver alone on use
lives-ok {
    EVAL 'use VersionedMod:ver<1.2.3>; versioned-greet()';
}, "use Module:ver<> alone parses and loads";

# :auth alone on use
lives-ok {
    EVAL 'use VersionedMod:auth<zef:mutsu-test>; versioned-greet()';
}, "use Module:auth<> alone parses and loads";

# A real import tag still works alongside (negative: unknown tag still errors)
throws-like {
    EVAL 'use VersionedMod:ver<1.2.3>:nonesuch; ';
}, X::Import::NoSuchTag,
    "genuine unknown import tag still rejected when mixed with :ver";
