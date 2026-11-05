use Test;
use experimental :rakuast;

# #14916: Doc::Markup.check-meta and .splat-letterless. On a node built by hand
# they report and change nothing; only the answers are observable. Rakudo answers
# `Nil` from check-meta for the letters whose handler ends in a check of its own
# (A D E F L M P X) and `Empty` for every other letter, with or without `meta`.

plan 10;

sub markup($letter, :$meta) {
    $meta ?? RakuAST::Doc::Markup.new(:$letter, atoms => ('x',), meta => ('m',))
          !! RakuAST::Doc::Markup.new(:$letter, atoms => ('x',))
}

for <B C> -> $l {
    is markup($l).check-meta.raku, 'Empty', "check-meta of $l is Empty";
}
is markup('L').check-meta.raku, 'Nil', 'check-meta of L is Nil';
is markup('E', :meta).check-meta.raku, 'Nil', 'check-meta of E with meta is Nil';
is markup('B', :meta).check-meta.raku, 'Empty', 'check-meta of B with meta is Empty';
is markup('').check-meta.raku, 'Empty', 'check-meta of the empty letter is Empty';
is (<A D E F L M P X>.map({ markup($_).check-meta.raku }).unique).join(','), 'Nil', 'the eight checked letters';
is (<B C G H I J K N O Q R S T U V W Y Z>.map({ markup($_).check-meta.raku }).unique).join(','), 'Empty', 'every other capital';

my $m = markup('B');
my $before = $m.raku;
is $m.splat-letterless.raku, 'Nil', 'splat-letterless answers Nil';
is $m.raku, $before, 'and leaves the node alone';
