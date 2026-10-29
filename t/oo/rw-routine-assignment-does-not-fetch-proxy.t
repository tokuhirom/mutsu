use Test;

plan 3;

# `f(...) = v` on an `is rw` routine runs the Proxy's STORE; the statement's
# value is the container, which a sunk assignment never reads, so FETCH stays
# silent (also when the routine is a multi reached through a proto).
my @log;
sub plain(Str:D \s) is rw {
    Proxy.new(FETCH => -> $ { @log.push: 'FETCH'; 1 }, STORE => -> $, \new { @log.push: "STORE {new}" })
}
proto sub viaproto(|) {*}
multi sub viaproto(Str:D \s) is rw {
    Proxy.new(FETCH => -> $ { @log.push: 'FETCH'; 1 }, STORE => -> $, \new { @log.push: "STORE {new}" })
}
my $name = 'freddy';

plain($name) = 'a';
is @log.join(','), 'STORE a', 'a plain is rw sub: STORE only';
@log = ();
viaproto($name) = 'b';
is @log.join(','), 'STORE b', 'an is rw multi through a proto: STORE only';
@log = ();
is (plain($name) = 'd'), 1, 'but the value reads through FETCH when it is used';
