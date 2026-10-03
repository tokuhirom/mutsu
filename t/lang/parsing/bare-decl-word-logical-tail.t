use Test;

# A loose word-logical (`and`, `or`, `andthen`, `orelse`, ...) after a
# declaration with no initializer applies to the declared variable:
# `my %h andthen do { ... }` is `(my %h) andthen do { ... }`. Found via the
# WebService::Slack::Webhook distribution, whose dependency Net::HTTP parses
# response headers with `my %header andthen do { ... for ... }`.

plan 7;

my %h andthen do { %h{$_} = 1 for <a b> };
is-deeply %h, %(a => 1, b => 1), 'andthen after a bare hash declaration';

my @a andthen @a.push(3);
is-deeply @a, [3], 'andthen after a bare array declaration';

my $ran = 0;
my $x and $ran = 1;
is $ran, 0, 'and short-circuits on the undefined scalar';

my $y or $ran = 2;
is $ran, 2, 'or runs its right side';

my $z orelse $ran = 3;
is $ran, 3, 'orelse runs its right side for an undefined scalar';

my Int $t andthen $ran = 4;
is $ran, 3, 'andthen skips its right side for a typed type object';

my $w andthen $ran = 5 if True;
is $ran, 3, 'a statement modifier still applies to the whole statement';
