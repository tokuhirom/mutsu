use nqp;
use Test;

plan 4;

# `nqp::strtocodes` decomposes a plain Str where it lies (no copy of its
# text); every shape below gives rakudo 2026.11's codepoints.

sub codes($s, $form) {
    my $u := nqp::strtocodes($s, $form, nqp::create(NFD));
    my @c;
    my int $i = -1;
    @c.push(nqp::atpos_i($u, $i)) while ++$i < nqp::elems($u);
    @c.join(',')
}

is codes('abc', nqp::const::NORMALIZE_NFD), '97,98,99', 'a plain Str';
is codes("\x[e9]", nqp::const::NORMALIZE_NFD), '101,769', 'decomposed to NFD';
is codes("e\x[301]", nqp::const::NORMALIZE_NFC), '233', 'composed to NFC';
my $v = 'xy';
is codes($v, nqp::const::NORMALIZE_NFD), '120,121', 'a Str held in a variable';
