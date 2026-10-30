unit module ForCodeParamModule;

# Humming-Bird::Core shape: an exported routine loops over callables bound to
# a `&middleware` pointy parameter, and the module also declares
# `multi sub middleware`. The loop's `&middleware` is the lexical.
sub run-all(@callables) is export {
    my @out;
    for @callables -> &middleware {
        @out.push: &middleware(7, 'r');
    }
    @out
}

sub map-all(@callables) is export {
    @callables.map(-> &middleware { &middleware(8) }).list
}

multi sub middleware(@m) is export { 'array' }
multi sub middleware(&m) is export { 'code' }
