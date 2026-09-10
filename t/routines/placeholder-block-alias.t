use Test;

plan 1;

sub showkv($x) {
    $x.keys.sort.map({ $^k ~ ':' ~ $x{$k} }).join(' ')
}

my $m := <a b c d e a b>.Mix;
is showkv($m), 'a:2 b:2 c:1 d:1 e:1', 'de-careted placeholder name resolves to $^k';
