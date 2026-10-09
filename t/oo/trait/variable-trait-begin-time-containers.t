use v6;
use Test;

# From Trait::Env t/09-basic-variable.rakutest: `@`/`%` variable traits run
# while the declaring block is compiled, so each sees the BEGIN before it.

plan 3;

multi sub trait_mod:<is>(Variable $v, :$env) {
    my $key = $v.name.substr(1).uc;
    $v.var = do given $v.var.WHAT {
        when Positional   { (%*ENV{$key} // '').split(':').map({ $_ }) }
        when Associative  { (%*ENV{$key} // '').split(':').map({ my ($k, $x) = .split('='); $k => $x }) }
        default           { %*ENV{$key} }
    }
}

sub run(&c) { c() }
my (@arrays, @hashes);

run({
    BEGIN { %*ENV = { "LIST" => "a:b", "MAP" => "x=1:y=2" }; }
    my @list is env;
    my %map is env;
    @arrays.push(@list.List);
    @hashes.push(%map.Hash);
});
run({
    BEGIN { %*ENV = { "LIST" => "c", "MAP" => "z=3" }; }
    my @list is env;
    my %map is env;
    @arrays.push(@list.List);
    @hashes.push(%map.Hash);
});

is-deeply @arrays, [('a', 'b'), ('c',)], '@ trait sees the BEGIN before the declaration';
is-deeply @hashes, [{ :x('1'), :y('2') }, { :z('3') }], '% trait sees the BEGIN before the declaration';
isa-ok @hashes[0], Hash, 'a Seq of Pairs assigned through .var becomes a Hash';
