use Test;

plan 7;

my $value = 2;

my $leaf = sub () is rw { $value };
my $forward = sub () is rw { $leaf() };

$forward() = 9;
is $value, 9, "anonymous rw sub assignment updates target";

my $readonly = sub () { $value };
dies-ok { $readonly() = 11 }, "non-rw anonymous sub assignment dies";
is $value, 9, "failed non-rw assignment does not modify value";

sub pass-ok($pw) { $pw eq "ok" }

sub guarded($pw) is rw {
    Proxy.new(
        FETCH => sub ($self) { $value },
        STORE => sub ($self, $new) {
            die "bad password" unless pass-ok($pw);
            $value = $new;
        },
    );
}

dies-ok { guarded("ng") = 12 }, "proxy rw sub store can fail";
is $value, 9, "failing proxy store keeps original value";
is (guarded("ok") = 13), 13, "proxy rw sub assignment returns fetched value";
is guarded("ok"), 13, "proxy rw sub fetch returns updated value";
