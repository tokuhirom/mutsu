use Test;

# A nested same-type `=begin` opens a block at its own indentation, and its
# `=end` closes it there, as in rakudo. Pod::Load's SYNOPSIS has an indented
# `=begin pod` / `=end pod` pair inside its `=begin pod`. #18369's
# own-indentation rule rejected that inner `=end` as misplaced, which left
# Pod::Load and the distributions using it unloadable.

plan 5;

my @ran;

=begin pod

    my $string-with-pod = q:to/EOH/;
    =begin pod
    This ordinary paragraph introduces a code block:
    =end pod
    EOH

=end pod

@ran.push('after pod');

=begin pod
  =begin pod
  inner
  =end pod
@ran.push('still pod');
=end pod

is-deeply @ran, ['after pod'], 'an indented nested pair is skipped with its block';

sub rakudoc-error(Str $code, Str $what) {
    my $err = (try { EVAL $code; Nil }) // $!;
    subtest $what, {
        plan 2;
        isa-ok $err, X::Syntax::InfixInTermPosition, 'the RakuDoc syntax error';
        like $err.message, /"Perhaps an '=end pod' marker"/, 'its message';
    }
}

rakudoc-error "=begin pod\n    =begin pod\n=end pod\nsay 2;\n",
    'an inner block left open at its own indentation';
rakudoc-error "=begin pod\n=begin pod\n  =end pod\n=end pod\nsay 3;\n",
    'an =end at another indentation than the innermost block';
rakudoc-error "=begin pod\nx\n  =end pod\nsay 42;\n=end pod\n",
    'an indented =end with no inner block (#18369)';

is (EVAL "=begin pod\n  =begin pod\n  =end pod\n=end pod\n42"), 42,
    'code after a nested pair runs';
