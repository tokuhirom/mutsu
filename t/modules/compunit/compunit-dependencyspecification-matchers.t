use Test;

# CompUnit::DependencySpecification normalizes its matchers as rakudo does: an
# undefined matcher is the default True, a version or api matcher becomes a
# Version, an auth matcher is kept as given (#17513: Red passes
# `:api-matcher(%qualifiers<api>)`, Any when absent, and the spec then matched
# no installed distribution).

plan 12;

sub spec(*%matchers) { CompUnit::DependencySpecification.new(:short-name<X>, |%matchers) }

is-deeply spec(:api-matcher(Any)).api-matcher, True, 'an undefined api matcher is True';
is-deeply spec(:version-matcher(Str)).version-matcher, True, 'an undefined version matcher is True';
is-deeply spec(:auth-matcher(Any)).auth-matcher, True, 'an undefined auth matcher is True';
is-deeply spec(:version-matcher<1.0>).version-matcher, v1.0, 'a version matcher string becomes a Version';
is-deeply spec(:api-matcher<2+>).api-matcher, v2+, 'an api matcher string becomes a Version';
is-deeply spec(:version-matcher(v1.2)).version-matcher, v1.2, 'a Version matcher stays';
is-deeply spec(:auth-matcher<zef:me>).auth-matcher, 'zef:me', 'an auth matcher is kept as given';
is-deeply spec(:auth-matcher(False)).auth-matcher, False, 'a False auth matcher is kept';
is spec(:api-matcher(Any), :version-matcher<1.0>).Str, 'X:ver<1.0>', '.Str names only the given matchers';
is spec(:auth-matcher<zef:me>, :api-matcher<2>).Str, 'X:auth<zef:me>:api<2>', '.Str with auth and api';
is spec().Str, 'X', 'no matchers';
ok v1.0 ~~ spec(:version-matcher<1.0+>).version-matcher, 'the Version matcher smartmatches a version';
