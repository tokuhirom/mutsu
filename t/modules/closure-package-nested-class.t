use Test;

plan 3;

# A routine value invoked from a foreign frame must run under its declaring
# package so nested-class short names resolve (JSON::Unmarshal 080:
# `is unmarshalled-by(&unmarshall-inners)` calling `Inner.new`).

class Outer {
    class Inner {
        has Str $.name;
    }

    our sub make-inners (@names) {
        @names.map(-> $name { Inner.new(:$name) })
    }
}

sub caller-elsewhere(&code, @args) { code(@args) }

my @r = caller-elsewhere(&Outer::make-inners, ["a", "b"]);
is @r.elems, 2, 'both elements built';
is @r[0].name, "a", 'nested class name resolved in foreign frame';
isa-ok @r[1], Outer::Inner, 'instances are of the nested class';
