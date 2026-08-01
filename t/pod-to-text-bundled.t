use v6;
use Test;

plan 5;

# `Pod::To::Text` is the genuine upstream module (rakudo's own core library,
# vendored verbatim at modules/Rakudo-Core/lib/Pod/To/Text.rakumod), not a
# native reimplementation. These assertions therefore pin real upstream
# rendering, character for character.

=begin pod

=head1 Top Heading

A paragraph of prose.

=item bullet one
=item bullet two

=end pod

use Pod::To::Text;

my $text = pod2text($=pod);

is $text.lines[0], 'Top Heading', 'a level-1 heading renders unindented';
ok $text.contains('A paragraph of prose.'), 'paragraph text survives';
ok $text.contains('  * bullet one'), '=item renders as an indented bullet';
ok $text.contains('  * bullet two'), 'every =item is rendered';

# `.render` is the class method Pod::To::Text exposes alongside `pod2text`;
# both must come from the module, so exercising it proves the class loaded.
is Pod::To::Text.render($=pod), $text, '.render agrees with pod2text';
