use Test;

# `my $x := <Proxy>` takes the env-free bind-declaration lane: the name is
# bound to the Proxy itself, so reads FETCH, writes STORE, and an outer
# same-named binding never receives either (#9244).

plan 9;

my $fetches = 0;
my $stored;
sub make-proxy() is raw {
    Proxy.new(
        FETCH => method () { $fetches++; 42 },
        STORE => method ($v) { $stored = $v },
    )
}

sub reads { my $p := make-proxy(); $p + $p }
is reads(), 84, 'reads of a bound Proxy FETCH';
ok $fetches > 0, 'reads FETCH';

sub writes { my $p := make-proxy(); $p = 'x'; $stored }
is writes(), 'x', 'an assignment STOREs through the bound Proxy';

class Holder {
    method proxy is raw { make-proxy() }
    method via-method { my $p := self.proxy; $p = 'm'; $stored }
}
is Holder.new.via-method, 'm', 'a Proxy returned by a method binds as itself';

sub loop-binds { my @seen; for ^3 -> $i { my $p := make-proxy(); $p = $i; @seen.push($stored) }; @seen.join(',') }
is loop-binds(), '0,1,2', 'a fresh Proxy bind per iteration';

{
    my $t = "ab";
    my $r := substr-rw($t, 0, 1);
    sub own-substr { my $s = "hello"; my $r := substr-rw($s, 0, 1); $r = "J"; $s }
    is own-substr(), "Jello", "the routine's own Proxy receives the store";
    is $t, "ab", 'the outer same-named Proxy is left alone';
}

{
    my $t = "ab";
    my $r := substr-rw($t, 0, 1);
    sub own-value { my $r := 5; try { $r = 6 }; ($r, $!.^name) }
    is-deeply own-value(), (5, 'X::AdHoc'), 'a value bind under an outer Proxy of the name stays readonly';
    is $t, "ab", 'the outer Proxy is not written';
}
