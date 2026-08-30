use Test;

plan 3;

=begin pod

=head1 Head one

Plain para with C<code>.

=end pod

is $=pod[0].gist, q:to/EXPECTED/.chomp,
Pod::Block::Named{:name("pod")}
  Pod::Heading{:level("1")}
    Pod::Block::Para
      Head one
  Pod::Block::Para
    Plain para with 
    Pod::FormattingCode{:type("C")}
      code
    .
EXPECTED
   'a named Pod block renders the indented pod-gist tree';

is $=pod.gist, '[Pod::Block::Named{:name("pod")}' ~ "\n"
    ~ '  Pod::Heading{:level("1")}' ~ "\n"
    ~ '    Pod::Block::Para' ~ "\n"
    ~ '      Head one' ~ "\n"
    ~ '  Pod::Block::Para' ~ "\n"
    ~ '    Plain para with ' ~ "\n"
    ~ '    Pod::FormattingCode{:type("C")}' ~ "\n"
    ~ '      code' ~ "\n"
    ~ '    .]',
   '$=pod.gist wraps the block tree in brackets';

my $empty = Pod::Block::Para.new(config => {}, contents => []);
is $empty.gist, 'Pod::Block::Para', 'empty Pod attributes are omitted';
