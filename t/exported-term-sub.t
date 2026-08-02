use v6;
use lib 't/lib';
use Test;
use ExportedTerm;

plan 5;

# An exported `sub term:<foo>` makes a bareword `foo` in the importing file a
# call to it. Only the operator categories (infix/prefix/postfix/circumfix/
# postcircumfix) were registered from a module's export list, so `term:<...>`
# fell through and the bareword parsed as a plain string.
# (Cro::HTTP::Router exports `term:<request>` and `term:<response>`; without
# this, `request` outside a handler evaluated to the string "request" instead
# of throwing X::Cro::HTTP::Router::OnlyInHandler.)

is answer, 42, 'a bareword calls the imported term sub';
is answer + 1, 43, 'and composes as an ordinary term in an expression';
isnt answer, 'answer', 'it is not parsed as a bareword string';

{
    my $*STATE = 'live';
    is current-state, 'live', 'the term sub runs at each use, seeing the dynamic scope';
}

dies-ok { current-state }, 'and it really is a call — it can throw';
