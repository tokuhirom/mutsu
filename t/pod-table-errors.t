use Test;

plan 2;

my $empty_begin_end = "=begin " ~ "table\n" ~ "=end " ~ "table\n";
my $mixed_separators = "=table\n1 | 2 | 3\n4  5  6\n";

eval-dies-ok $empty_begin_end, 'empty begin/end table dies';
eval-dies-ok $mixed_separators, 'mixed visible and invisible separators die';
