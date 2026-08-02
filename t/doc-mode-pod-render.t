use v6;
use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers");
use Test::Util;

plan 4;

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

my $pod-variable = q:to"END";
    #| routine docs
    sub documented(Int $value) { $value }

    DOC INIT {
        say $=pod.elems;
        say $=pod[0].WHEREFORE.^name;
        say $=pod[0].WHEREFORE.WHY === $=pod[0];
        say $=pod[0].WHEREFORE.signature.params.elems;
        exit;
    }
    END

is_run(
    $pod-variable,
    %( out => "1\nSub\nTrue\n1\n", err => ''),
    :compiler-args['--doc'],
    'DOC INIT sees $=pod with its concrete routine declarant'
);

# pod2text renders the Pod object tree
=begin pod

=head2 P2T Heading

P2T body text

=item bullet one

=end pod

use Pod::To::Text;
my $text = pod2text($=pod);
ok $text ~~ /'P2T Heading' .*? 'P2T body text' .*? '* bullet one'/, 'pod2text over $=pod';
