use v6;
use Test;

plan 4;

# `$=pod` is collected by scanning the source line by line, so it used to pick
# up Pod written *inside* a string literal. A heredoc body is program data, not
# program documentation: rakudo collects nothing from it.

=begin pod

=head1 The Real Heading

=end pod

my $embedded = q:to"END";
    =begin pod

    =head1 Not A Real Heading

    =end pod

    #| not a real declarator either
    class Decoy { }
    END

is $=pod.elems, 1, 'only the document outside the heredoc is collected';
is $=pod[0].^name, 'Pod::Block::Named', 'and it is the real =begin pod block';

use Pod::To::Text;
my $text = pod2text($=pod);
ok $text.contains('The Real Heading'), 'the real heading renders';
nok $text.contains('Not A Real Heading'), 'the heredoc heading does not';

# (The heredoc itself is of course still an ordinary string.)
$embedded.so;
