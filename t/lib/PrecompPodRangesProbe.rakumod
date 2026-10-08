unit module PrecompPodRangesProbe;

# Code before the Pod, a heredoc whose body looks like Pod (it is not), and
# Pod blocks of several shapes with code between them.
my $fake = q:to/END/;
=begin pod
not a pod block
=end pod
END

=begin pod
=head1 First

Some text.

=end pod

our $COUNT = $=pod.elems;
our $LINES = $fake.lines.elems;

=for comment
a paragraph comment

=head2 Abbreviated
text after

our $KINDS = $=pod.map(*.^name).join(',');

=begin table
a b
c d
=end table
