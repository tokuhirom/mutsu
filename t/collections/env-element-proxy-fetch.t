use Test;

# `%*ENV<KEY>` read by value FETCHes an element bound to a Proxy, as any
# other hash element read does, so a copy taken before the Proxy's backing
# store changes keeps the value it read. The Env distribution binds
# `%*ENV<PATH>` to such a Proxy for `use Env <@PATH>` (t/02-specific).

plan 6;

my @parts = <a b>;
%*ENV<MUTSU_TEST_PROXY_PATH> := Proxy.new(
    FETCH => -> $ { @parts.join(':') },
    STORE => -> $, \v { @parts = v.split(':') },
);

my $copy = %*ENV<MUTSU_TEST_PROXY_PATH>;
@parts.push: 'c';
is $copy, 'a:b', 'a literal-key %*ENV read FETCHes the Proxy once';
is %*ENV<MUTSU_TEST_PROXY_PATH>, 'a:b:c', 'a later read sees the new value';

my $key = 'MUTSU_TEST_PROXY_PATH';
my $by-var = %*ENV{$key};
@parts.push: 'd';
is $by-var, 'a:b:c', 'a variable-key %*ENV read FETCHes the Proxy once';

%*ENV<MUTSU_TEST_PROXY_PATH> = 'm:n';
is-deeply @parts, [<m n>], 'an assignment still STOREs through the Proxy';

my $plain = 'x';
%*ENV<MUTSU_TEST_BOUND_SCALAR> := $plain;
my $scalar-copy = %*ENV<MUTSU_TEST_BOUND_SCALAR>;
$plain = 'y';
is $scalar-copy, 'x', 'a copy of a scalar-bound element is a value';
is %*ENV<MUTSU_TEST_BOUND_SCALAR>, 'y', 'the bound element follows the scalar';
