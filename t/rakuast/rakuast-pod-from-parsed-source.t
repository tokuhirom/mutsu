use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# A parsed tree's `$=pod` is the Pod of the source it came from, also for the
# shapes that cannot be written back from Doc::Block nodes (table, comment).
plan 4;

my $table = Q[=table
X | O |

=table
A | B

$=pod.elems].AST;
is EVAL($table), 2, 'two =table blocks are two entries';

my $mixed = Q[=begin pod

=head1 Title

=end pod
$=pod[0].name].AST;
is EVAL($mixed), 'pod', 'a named block keeps its name';

my $comment = Q[=begin comment

raw text

=end comment
$=pod.elems].AST;
is EVAL($comment), 1, 'a comment block is an entry';

#| doc for a
sub a { }
my $docs = Q[#| doc for a
sub a { }
=begin pod

x

=end pod
$=pod.elems].AST;
is EVAL($docs), 2, 'a declarator block and a named block are both entries';
