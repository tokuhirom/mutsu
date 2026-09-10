use Test;

plan 4;

sub infix:<wtf>($a, $b) { $a ~ "WTF" ~ $b };

is ([wtf] <OMG BBQ PONIES>), "OMGWTFBBQWTFPONIES", "custom infix works in reduction";
is "BBQ" Rwtf "OMG", "OMGWTFBBQ", "reverse meta-op works for custom infix";
is ~(("OMG", "BBQ") Xwtf ("OMG", "BBQ")),
    "OMGWTFOMG OMGWTFBBQ BBQWTFOMG BBQWTFBBQ",
    "cross meta-op works for custom infix";
is ~(("OMG", "BBQ") >>wtf<< ("BBQ", "OMG")), "OMGWTFBBQ BBQWTFOMG",
    "hyper meta-op works for custom infix";
