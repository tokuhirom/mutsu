use Test;

plan 1;

my $out = "";
my $f = sub (\obj, \key) is rw {
    Proxy.new(
        FETCH => { "f" },
        STORE => -> $, $v { $out = "s:" ~ obj ~ ":" ~ key },
    )
};

($f("OBJ", "KEY")) = "V";
is $out, "s:OBJ:KEY",
    "a Proxy STORE closure captures sigilless parameters of an anonymous sub";
