use Test;
plan 5;

my %h = a => 2;
my $v1 = %h<a>++;
is $v1, 2, 'postfix ++ on hash index returns old value';
is %h<a>, 3, 'postfix ++ on hash index updates value';

my @a = 10, 20;
my $v2 = @a[1]--;
is $v2, 20, 'postfix -- on array index returns old value';
is @a[1], 19, 'postfix -- on array index updates value';

throws-like '(1 + 2)++', X::Multi::NoMatch,
    'postfix ++ on unsupported target throws X::Multi::NoMatch';
