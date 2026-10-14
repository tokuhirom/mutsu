# A plain (non-`unit`) module whose `sub EXPORT` selects names through
# `UNIT::`, as path-utils does (#13272).
my constant &us-alpha = -> $x { "alpha:$x" };
my constant &us-beta  = -> $x { "beta:$x" };
my $us-plain = 'plain';
my sub us-helper { 'helper' }

my sub EXPORT(*@names) {
    my $direct = UNIT::{"&us-alpha"}:exists;
    my @in-closure = <us-alpha us-beta>.map: { UNIT::{"&$_"}:exists };
    my @keys = UNIT::.keys.grep({ .starts-with('&us-') || $_ eq '$us-plain' }).sort;
    Map.new: |(@names
        ?? @names.map({ UNIT::{"&$_"}:p })
        !! ()),
        '&unit-stash-report' => -> { "$direct|{@in-closure}|{@keys}" };
}
