use Test;

# A copied named hash parameter materializes the entries inside
# :options(...), rather than retaining the nested Pair itself.
# WebService::TMDB uses this shape for request query parameters.
plan 2;

sub named-hash(:%options is copy) {
    %options.^name ~ ':' ~ %options<query>
}

is named-hash(:options(query => 'value')), 'Hash:value',
    'named hash parameters materialize a Hash';
is named-hash(:options(query => 'value', page => 2)), 'Hash:value',
    'named hash parameters retain all nested entries';
