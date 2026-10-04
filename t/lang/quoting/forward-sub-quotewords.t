use Test;
use lib 't/lib';
use WordListNamedImport;
plan 6;

# Reduced from Test::Run 0.2.3's forward test_runs_ok call and :args«…» import.
my $guillemet = words «alpha beta»;
is $guillemet, 'alpha,beta', 'guillemet words start a forward call argument';
my $angle = words <red blue>;
is $angle, 'red,blue', 'angle words start a forward call argument';
my $double_angle = words <<one two>>;
is $double_angle, 'one,two', 'double-angle words start a forward call argument';
ok 2 < 3, 'less-than remains an infix operator';
is word_join(:args«green yellow»), 'green,yellow', 'parenthesized imported named word list';
my $named = word_join :args«green yellow»;
is $named, 'green,yellow', 'statement-level imported named word list';

sub words(*@items) { @items.join(',') }
