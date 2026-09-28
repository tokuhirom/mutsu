use Test;

# An indented paragraph inside `=begin pod` is an implicit code block only
# when it is indented further than the *virtual margin* -- the indentation
# of the last text-bearing directive (rakudo's `$*VMARGIN`). Found through
# Pod::To::HTML's t/020-code.t.

plan 8;

sub shape($block) {
    $block.contents.map({ .^name.subst('Pod::Block::', '') ~ ':' ~ .contents.join('|') }).List
}

=begin pod
A

    =head1 H

    B at the heading's indentation

        C deeper
=end pod
is-deeply shape($=pod[0])[2, 3],
    ("Para:B at the heading's indentation", "Code:C deeper"),
    'an indented =head1 moves the margin';

=begin pod
    =head1 H

B zero

    C four
=end pod
is shape($=pod[1])[2], 'Para:C four', 'the margin persists after an unindented paragraph';

=begin pod
    =begin code
    k
    =end code

  A two
=end pod
is shape($=pod[2])[1], 'Code:A two', 'a nested =begin block does not move the outer margin';

=begin pod
    =comment hi

  B two
=end pod
is shape($=pod[3])[1], 'Code:B two', '=comment does not move the margin';

=begin pod
    =head1 x

=head2 y

  C two
=end pod
is shape($=pod[4])[2], 'Code:C two', 'an unindented directive resets the margin to 0';

=begin pod
    =for head1
    x

  D two
=end pod
is shape($=pod[5])[1], 'Para:D two', 'the =for form of a text block moves the margin too';

=begin pod
A
    B
=end pod
is-deeply shape($=pod[6]), ('Para:A B',), 'continuation lines of a paragraph are never code';

=begin pod
This is an ordinary paragraph

    While this is not
    This is a code block

    =head1 Mumble: "mumble"

    Suprisingly, this is not a code block
        (with fancy indentation too)

But this is just a text. Again
=end pod
is-deeply shape($=pod[7])[0, 1, 3, 4], (
    'Para:This is an ordinary paragraph',
    "Code:While this is not\nThis is a code block",
    'Para:Suprisingly, this is not a code block (with fancy indentation too)',
    'Para:But this is just a text. Again',
), 'Pod::To::HTML t/020-code.t fixture';
