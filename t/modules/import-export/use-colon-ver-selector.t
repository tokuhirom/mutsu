use Test;

plan 4;

# `use Foo:ver:<0.0.5>` — the version/auth/api selector may be written with a
# colon before its angle value (`:ver:<...>`), not just `:ver<...>`. Some lizmat
# dists (e.g. Acme::Cow) use this form. Both must parse; the `:<...>` colon is
# optional. We assert by round-tripping through EVAL: a genuine parse error would
# throw, a successful parse of a `use` on a missing module throws a *load* error
# (X::CompUnit / "Could not find"), never a parse error.

sub parse-ok(Str $code) {
    my $err = '';
    try {
        EVAL $code;
        CATCH { default { $err = .Str } }
    }
    # A parse failure surfaces as X::Syntax / "Undeclared routine"; a load
    # failure ("Could not find") means the `use` parsed fine.
    $err eq '' || $err.contains('Could not find') || $err.contains('CompUnit');
}

ok parse-ok('use NoSuchModule::ForTest:ver:<0.0.5>:auth<zef:lizmat>;'),
    'use Foo:ver:<0.0.5>:auth<...> parses (colon before angle value)';

ok parse-ok('use NoSuchModule::ForTest:ver<0.0.5>:auth<zef:lizmat>;'),
    'use Foo:ver<0.0.5>:auth<...> still parses (plain form)';

ok parse-ok('use NoSuchModule::ForTest:auth:<zef:lizmat>;'),
    'use Foo:auth:<...> parses (colon before angle value)';

ok parse-ok('use NoSuchModule::ForTest:ver:<0.0.5>;'),
    'use Foo:ver:<0.0.5> parses (single selector)';
