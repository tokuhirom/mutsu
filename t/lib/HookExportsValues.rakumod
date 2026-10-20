# A `sub EXPORT` hook that exports a scalar, an array, a sigilless term and
# a routine under literal keys (ADR-8751, #14140).
my @log = <a b>;
sub EXPORT {
    %(
        '$hv'  => 'hv-val',
        '@hl'  => @log,
        'ht'   => 'term-val',
        '&hf'  => sub { 'hf' },
    )
}
