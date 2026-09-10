use Test;

plan 8;

# `native` is a package declarator peer to `class`/`role`/`grammar` that
# rakudo's NativeCall::Types uses to define native scalar types, e.g.
# `native long is Int is ctype<long> is repr<P6int> { }`. mutsu had no
# statement-level handling for the `native` keyword at all, so `our native
# long is Int is ctype<long> is repr<P6int> { }` failed with
# `X::Syntax::Malformed: Malformed my`. This is unrelated to full NativeCall
# support: `native` parses like `class` (same `is`/`does`/`hides` loop, same
# body), it's just a different declarator keyword.

native mynativeint is Int is ctype<long> is repr<P6int> { }
is mynativeint.^name, 'mynativeint', 'bare "native Name is Type { }" declares a type';
is mynativeint.^parents(:all).map(*.^name).grep('Int').elems, 1,
    '"native Name is Type" makes Type a parent, same as "class Name is Type"';

our native mynativebyte is Int { }
is mynativebyte.^name, 'mynativebyte', '"our native Name is Type { }" also parses';

my $r = do {
    my native mynativeshort is Int { }
    mynativeshort.^name;
};
is $r, 'mynativeshort', '"my native Name is Type { }" also parses';

# --- trait_mod:<is> dispatch: `ctype` (no dedicated field) still reaches
# user code from a `native` declaration, same as it does from a class.

my @log;
multi sub trait_mod:<is>(Mu:U $t, :$ctype!) { @log.push("{$t.^name}:ctype:{$ctype}") }

native mynativelong is Int is ctype<long> { }
ok @log.grep('mynativelong:ctype:long'),
    'native: is ctype<long> reaches trait_mod:<is>, same as a class';

# --- an ordinary method still works on a `native`-declared type, confirming
# the body parses exactly like a class body (not just the declarator line).

native mynativecounter is Int {
    method describe() { "native:ok" }
}
is mynativecounter.new.describe, 'native:ok', 'a native-declared type has a working body/methods';

# --- sanity: `native` used as a sub trait (`is native('libname')`) in its
# existing, unrelated position is unaffected by the new statement-level
# `native` keyword — it must not be misparsed as the package declarator.
sub some_c_call() is native('libc') { * }
ok True, '"is native(...)" as a sub trait still parses (no collision)';

is 1 + 1, 2, 'sanity: ordinary code after native declarations still parses';
