use Test;

# `.raku` of a lazy list renders its prefix as Rakudo's `List.raku` does, through
# `my @elements = self.head(101)`: every element is read back out of an
# Array's Scalar, so a positional, a hash or a Seq renders itemized and `Nil`
# as `Any` (#16139). What a consumer pulls is unchanged.

plan 9;

sub mk { (lazy gather { take 1; take 2 }) X (3, 4) }

is mk().raku, '($(1, 3), $(1, 4), $(2, 3), $(2, 4)).lazy.Seq', 'rows of a lazy X product';
my $s = mk();
is $s.raku, '$(($(1, 3), $(1, 4), $(2, 3), $(2, 4)).lazy.Seq)', 'an itemized lazy Seq';
is mk().cache.raku, '($(1, 3), $(1, 4), $(2, 3), $(2, 4)).lazy', 'a lazy List view';
is ((1, 2), (3, 4)).lazy.raku, '($(1, 2), $(3, 4)).lazy.Seq', 'a lazy list of lists';
is (1, (2, 3), [4, 5], "a", (6,)).lazy.raku, '(1, $(2, 3), $[4, 5], "a", $(6,)).lazy.Seq',
    'scalars stay bare, positionals itemize';
is ({a => 1}, :b(2), 3.5, Nil).lazy.raku, '(${:a(1)}, :b(2), 3.5, Any).lazy.Seq',
    'a hash itemizes, a Pair does not, Nil reads as Any';

is mk().head(2).raku, '((1, 3), (1, 4)).Seq', '.head drops the itemization';
is mk().flat.raku, '(1, 3, 1, 4, 2, 3, 2, 4).lazy.Seq', 'the rows still flatten';
is ((1, 2), (3, 4)).Seq.raku, '((1, 2), (3, 4)).Seq', 'a non-lazy Seq is unchanged';
