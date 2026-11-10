use Test;

plan 3;

# A key made of a letter plus combining marks is an identifier, so Hash.raku
# and Pair.raku print it in the adverbial `:key(value)` form.
my $late  = "a\x[303]\x[301]";
my $early = "a\x[301]\x[302]\x[303]";
my %h = $late => 1, $early => 2;
is %h.raku, "\{:$early\(2), :$late\(1)}", 'Hash.raku of combining-mark keys';
is ($early => 2).raku, ":$early\(2)", 'Pair.raku of a combining-mark key';
is-deeply %h, {$early => 2, $late => 1}, 'the hash compares equal to a literal with the same keys';

done-testing;
