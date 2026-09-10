use Test;

plan 8;

sub blk(&b) { b() }

my $p = 'outer';
blk { my $v = 100; (my $p := foo => $v).WHICH };
is $p, 'outer', 'expression-position scalar bind shadows an outer lexical';

my $x = 'outer';
blk { (my $x = 1) + $x };
is $x, 'outer', 'expression-position scalar assignment shadows an outer lexical';

my @a = <outer>;
blk { (my @a = <inner value>).elems };
is @a.raku, '["outer"]', 'expression-position array shadows an outer lexical';

my %h = outer => 1;
blk { (my %h = inner => 2)<inner> };
is-deeply %h, { outer => 1 }, 'expression-position hash shadows an outer lexical';

my $captured = 'outer';
sub captured-probe() { (my $captured = 'inner') }
is captured-probe(), 'inner', 'expression declaration yields its local value';
is $captured, 'outer', 'expression declaration does not write through a captured cell';

{
    my $promoted = 3;
    {
        is (* + (my $promoted = 5)).(8), 13,
            'WhateverCode sees the expression-position declaration';
        is $promoted, 5, 'a following statement sees the inner-block declaration';
    }
}
