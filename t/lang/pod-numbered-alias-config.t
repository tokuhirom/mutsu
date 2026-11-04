use Test;

plan 9;

# The `#` abbreviation of a block's `:numbered` configuration is the Int 1 and
# is not part of the text; an explicit `:numbered` stays True (as in rakudo).
=begin pod
=head1 # Title
=item # Numbered one
=item2 # Nested
=item Plain
=para # Para text

=for para :numbered
explicit
=end pod

my @c = $=pod[0].contents;
is-deeply @c[0].config<numbered>, 1, '=head1 # is numbered(1)';
is-deeply @c[1].config<numbered>, 1, '=item # is numbered(1)';
is @c[1].contents[0].contents[0], 'Numbered one', 'the # is not part of the item text';
is-deeply @c[2].config<numbered>, 1, '=item2 # is numbered(1)';
is @c[2].level, 2, 'the item level is kept';
nok @c[3].config<numbered>.defined, 'a plain item has no numbered config';
is-deeply @c[4].config<numbered>, 1, '=para # is numbered(1)';
is @c[4].contents[0].contents[0], 'Para text', 'the # is not part of the para text';
is-deeply @c[5].config<numbered>, True, 'explicit :numbered is True';
