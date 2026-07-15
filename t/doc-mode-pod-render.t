use v6;
use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers");
use Test::Util;

plan 3;

# --doc renders pod blocks in source order BEFORE declarator blocks, and
# renders =item bullets (advent2011-day10).

my $main = q:to"END";
    =begin pod

    =head1 A Heading!

    A paragraph!

    =item A list!

    =end pod

    #| it's a sheep! really!
    class Sheep {
        #| produces a funny sound
        method bark { say "no" }
    }
    END

my $expected = rx/'A Heading!'
       .*? 'A list!'
       .*? 'class Sheep' .*? "it's a sheep! really!"
       .*? 'method bark' .*? 'produces a funny sound'/;

is_run($main, %( out => $expected, err => ''), :compiler-args['--doc'], '--doc order and items');

my $main2 = $main ~ q:to"--END--";

    DOC INIT {
        use Pod::To::Text;
        pod2text($=pod);
    }
    --END--

is_run($main2, %( out => $expected, err => ''), :compiler-args['--doc'], '--doc + DOC INIT');

# pod2text renders the Pod object tree
=begin pod

=head2 P2T Heading

P2T body text

=item bullet one

=end pod

use Pod::To::Text;
my $text = pod2text($=pod);
ok $text ~~ /'P2T Heading' .*? 'P2T body text' .*? '* bullet one'/, 'pod2text over $=pod';
