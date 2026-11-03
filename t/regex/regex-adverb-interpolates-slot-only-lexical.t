use Test;

# A regex with adverbs (`m:i/$re/`, `m:g/$re/`) names a lexical that nothing
# else has read yet: the variable must reach the pattern, not become `<!>`.
# From Geo::Ellipsoid's `$unit ~~ m:i/^$re/`.

plan 11;

my $re = "met";
ok ("meter" ~~ m:i/^$re/).so, 'm:i/^$re/ at file scope';
ok ("MeTer" ~~ m:i/$re/).so, 'm:i matches regardless of case';
ok ("met" ~~ m:g/$re/).so, 'm:g/$re/ at file scope';

sub in-sub($unit) {
    my $r = "met";
    so $unit ~~ m:i/^$r/
}
ok in-sub("meter"), 'inside a sub';

sub in-loop($unit) {
    my $conversion = 0;
    my %distance = (foot => 0.3048, meter => 1.0);
    for %distance.kv -> $key, $val {
        my $x = $key.substr(0, 3);
        if $unit ~~ m:i/^$x/ {
            $conversion = $val;
            last;
        }
    }
    $conversion
}
is in-loop("meter"), 1, 'a loop-local pattern variable';
is in-loop("foot"), 0.3048, 'and the other key';

my $a = "met";
my $r = "meter" ~~ m:i/^$a/;
is ~$r, "met", 'assigned match result';
nok ("xyz" ~~ m:i/$a/).so, 'a non-matching subject still fails';
ok ("met" ~~ m:s/$a/).so, 'm:s/$re/';
ok ("met" ~~ m:ignorecase/$a/).so, 'm:ignorecase/$re/';

{
    my $inner = "ABC";
    ok ("abc" ~~ m:i/$inner/).so, 'block-scoped variable';
}

done-testing;
