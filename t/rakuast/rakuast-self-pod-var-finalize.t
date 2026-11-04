use v6;
use Test;

plan 5;

# The user lexical `$self` and the Pod variable `$=pod` are carried through
# finalization as the parser built them (ADR-15760 slice 2).

class D { method both { my $f = sub ($self) { $self ~ self.^name }; $f("x") } }
is D.new.both, 'xD', 'sub ($self) inside a method: parameter and invocant stay apart';

sub g($self, $y) { "$self-$y" }
is g(1, 2), '1-2', 'a $self parameter of a plain routine';

my $self = 'lex';
is $self, 'lex', 'a my $self lexical';

=begin pod
text
=end pod

is $=pod.elems, 1, '$=pod holds the unit\'s pod';
is $=pod[0].^name, 'Pod::Block::Named', '$=pod[0] is the named block';
