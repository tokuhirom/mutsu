use Test;

plan 7;

# A method always carries an implicit `*%_`, so the legacy argument variable
# `%_` is a valid lexical anywhere in the method body -- including inside a
# nested signature-less `do {}` block. This blocked DBIish, whose
# DBIish.install-driver uses `|%_` inside a `do {}` block nested in a
# `.protect: { ... }` block.
#
# `@_` is NOT part of this: `raku` only auto-adds `*%_` to a method, never
# `*@_` -- referencing `@_` in a method body (directly, or nested in a `do
# {}`) is a compile-time error there (`raku -e 'class A { method m {
# @_.raku.say } }'` => "Placeholder variables (eg. @_) cannot be used in a
# method. Please specify an explicit signature, like method m (*@_) { ... }").
# See raku-doc/doc/Language/signatures.rakudoc ("Methods automatically get a
# *%_...") and Type/Method.rakudoc (the `nextsame`-forwarding rationale for
# why only named extras are silently captured).

# %_ directly inside a do {} block in a method
{
    class A { method m { my $r = do { %_ }; $r } }
    is A.m(a => 1, b => 2).elems, 2, '%_ in do {} inside method resolves named args';
}

# @_ directly inside a do {} block in a method is still forbidden -- a method
# gets no implicit *@_, unlike %_. (The class declaration alone does not
# trigger it -- mutsu raises this when the method body actually runs, unlike
# `raku`'s compile-time SORRY -- so the probe calls the method.)
throws-like 'class B { method m { my $r = do { @_ }; $r.elems } }; B.new.m',
    X::Placeholder::Block,
    '@_ in do {} inside method is still forbidden (no implicit *@_)';

# %_ in a do {} nested inside a bare block (the DBIish .protect: shape)
{
    class C {
        method install {
            my $lock = Lock.new;
            $lock.protect: {
                my $d = do { self.make(|%_) };
                $d;
            }
        }
        method make(*%a) { %a.elems }
    }
    is C.new.install(x => 1, y => 2, z => 3), 3,
        '%_ in do {} inside a nested block in a method';
}

# %_ used directly in a method (no do block) still works
{
    class D { method m { %_.elems } }
    is D.m(a => 1), 1, '%_ directly in method body';
}

# A plain sub still rejects %_ inside a nested do {} block (no implicit slurpy)
throws-like 'sub f { do { %_ } }; f(a => 1)', X::Placeholder::Block,
    '%_ in do {} inside a sub is still forbidden';

# %_ at the mainline is still an error
throws-like 'my $x = do { %_ }', X::Placeholder::Block,
    '%_ in a mainline do {} is still forbidden';

# %_ directly in a sub body still works (auto-signature)
{
    sub g { %_.elems }
    is g(a => 1, b => 2), 2, '%_ directly in a sub body works';
}
