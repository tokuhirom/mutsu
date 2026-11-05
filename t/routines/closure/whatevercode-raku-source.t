use Test;

# #16654 (found by WhereList via Testo, whose default description is
# "{$got.raku} is {$exp.raku}"): rakudo (RakuAST) renders a WhateverCode by the
# source it was written as.

plan 14;

my $not = *.not;
is $not.raku, '*.not', '.raku of *.not';
is $not.gist, '*.not', '.gist of *.not';

my $add = *+1;
is $add.raku, '*+1', 'the source text is kept verbatim (no spaces)';
my $add2 = * + 1;
is $add2.raku, '* + 1', 'the source text is kept verbatim (spaces)';

my $cmp = *.chars  >=  3;
is $cmp.raku, '*.chars  >=  3', 'internal whitespace is kept';
my $uni = *.chars ≥ 3;
is $uni.raku, '*.chars ≥ 3', 'a Unicode infix keeps its spelling';

my $two = * + *;
is $two.raku, '* + *', 'a two-argument WhateverCode';

my $colon = *.contains: any <o a e>;
is $colon.raku, '*.contains: any <o a e>', 'a colon method call';

my $idx = *[0];
is $idx.raku, '*[0]', 'a subscript';

my $cat = ('a' ~ *);
is $cat.raku, "'a' ~ *", 'a parenthesised term renders without the parens';

is [*.not, * + 1].raku, '[*.not, * + 1]', 'inside an Array';
is [*.not, * + 1].gist, '[*.not * + 1]', 'inside an Array .gist';

is (*.chars ≥ 3).WHAT.gist, '(WhateverCode)', 'the type is unchanged';
is $not(False), True, 'the closure still works';
