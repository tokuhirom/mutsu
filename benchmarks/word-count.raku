# Word-frequency counting: string-keyed hash increments over repeated text.
my @vocab = <the quick brown fox jumps over lazy dog and cat sat on mat
             with some more words that repeat across every single line
             counting frequencies is a classic small text processing task>;

my @lines;
for ^50 -> $i {
    my @ws;
    for ^20 -> $j {
        @ws.push(@vocab[($i * 7 + $j * 3) % @vocab.elems]);
    }
    @lines.push(@ws.join(' '));
}

my %counts;
for ^100 {
    for @lines -> $line {
        for $line.split(' ') -> $w {
            %counts{$w}++;
        }
    }
}
say %counts.elems, ' ', [+] %counts.values;
