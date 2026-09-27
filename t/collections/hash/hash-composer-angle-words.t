use Test;

# A `{ ... }` body that opens with a pair is a hash composer unless it
# references the topic. A `.word` inside a `< ... >` word list is literal text,
# not an implicit-topic method call, so it must not turn the body into a block.
# Found via the Math::Symbolic distribution, whose operation table holds
# `{ :language<raku>, :type<postfix>, :parts< .abs > }` and then does
# `Syntax.new(|%$_)` on it (which died with "Odd number of elements").

plan 6;

is { :type<postfix>, :parts< .abs > }.^name, 'Hash', 'colonpair word list with a .word';
is { a => < .abs .sign > }.^name, 'Hash', 'fat-arrow word list with .words';
is-deeply { :parts< .abs > }<parts>, '.abs', 'the word list value is kept';

my %h = |%({ :type<postfix>, :parts< .abs > });
is %h<type>, 'postfix', 'flattening the composed hash works';

# Infix less-than is not a word list, so a topic call after it still counts.
is { a => 1 < .elems }.^name, 'Block', 'topic call after infix < forces a block';
is { a => 1 <= .elems }.^name, 'Block', 'topic call after infix <= forces a block';
