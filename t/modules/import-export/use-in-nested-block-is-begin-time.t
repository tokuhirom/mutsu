# Raku performs `use` at BEGIN time, so a module `use`d inside a block that has
# not run yet is still visible to code that runs first. mutsu compiles `use` to
# a runtime `OpCode::UseModule`, so the load used to happen only when control
# reached the block -- and because an unresolved `::`-qualified bareword is
# answered with a fabricated stub rather than an error, the divergence was
# silent: `X::BeginUse::Marker.^name` answered the *written* name instead of the
# module-composed one (GH #8201).
#
# The fix hoists only the *load* (`OpCode::PreloadModule`, `compiler::begin_use`).
# The import stays at the `use`'s own run position, where Raku scopes it -- the
# constraint `roast/S11-modules/lexical.t` pins and this file re-asserts below.
use lib 't/lib';
use Test;

plan 5;

# The issue's repro: the `use` is earlier in FILE order but later in RUN order.
my &later = { use BeginUseFixture; };

is X::BeginUse::Marker.^name, 'BeginUseFixture::X::BeginUse::Marker',
   'a type from a module used in a not-yet-run block resolves to its composed name';

ok X::BeginUse::Marker ~~ Exception,
   'the preloaded type is the real type object, not a fabricated stub';

# The load is hoisted; the import is not.
nok defined(::('&begin-use-probe')),
    'the nested use does not leak its import into the enclosing scope';

later();

# `throws-like`'s two arguments both evaluate before the block is invoked, so
# the type reference is read before the block's `use` has run. This is the shape
# that kept JSON::Tiny's upstream `t/01-parse.t` at 92/93.
throws-like {
    use BeginUseFixture;
    die X::BeginUse::Marker.new,
}, X::BeginUse::Marker;

# Raku rejects an unresolvable `use` at BEGIN time whatever block it sits in;
# mutsu deliberately does not (a missing module only fails where the `use`
# actually runs). The preload must not quietly change that: it discards its own
# load failure, so hoisting cannot turn a program that used to run into one that
# dies before its first statement.
my &never = { use No::Such::Module::For::BeginUse::Preload; };
ok &never.defined, 'a preload that cannot resolve its module does not abort the unit';
