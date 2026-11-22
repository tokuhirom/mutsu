use Test;

plan 9;

# A delimited Pod block ends at the `=end` written at its `=begin`'s own
# indentation, as in rakudo (#18369). What the parser skips decides what runs.

my @ran;

=begin code
  x
  =end code
@ran.push('inside code');
=end code

@ran.push('after code');

=begin comment
  =end comment
@ran.push('inside comment');
=end comment

=begin pod
  =begin code
  @ran.push('nested code');
  =end code
=end pod

is-deeply @ran, ['after code'], 'an indented =end is text of a verbatim block';

=begin pod
=begin pod
inner
=end pod
@ran.push('still pod');
=end pod

is-deeply @ran, ['after code'], 'a nested same-type block at the same indentation';

sub compiles-with(Str $code, Str $what) {
    my $err = (try { EVAL $code; Nil }) // $!;
    subtest $what, {
        plan 2;
        isa-ok $err, X::Syntax::InfixInTermPosition, 'the RakuDoc syntax error';
        like $err.message, /'An error in RakuDoc syntax is suspected'/, 'its message';
    }
}

compiles-with "=begin pod\nx\n  =end pod\nsay 42;\n=end pod\n",
    'an indented =end inside a non-verbatim block';
compiles-with "  =begin pod\nx\n=end pod\nsay 42;\n  =end pod\n",
    'an unindented =end inside an indented block';
compiles-with "=begin code\n=begin code\n=end code\nsay 42;\n=end code\n",
    'a code block does not nest';
compiles-with "say 1;\n=end pod\nsay 2;\n", 'an =end no =begin opened';

my $e = (try { EVAL "say 1;\n=end pod\n"; Nil }) // $!;
like $e.message, /"Perhaps a '=begin pod' marker"/, 'an orphan =end names its =begin';
$e = (try { EVAL "=begin pod\n  =end pod\n=end pod\n"; Nil }) // $!;
like $e.message, /"Perhaps an '=end pod' marker"/, 'a misplaced =end names itself';
is (EVAL "=begin code\n  =end code\n=end code\n42"), 42, 'code after the block runs';
