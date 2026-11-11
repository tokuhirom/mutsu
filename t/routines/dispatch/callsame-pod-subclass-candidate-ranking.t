use Test;

# `callsame` from a subclass's `multi method start(Pod::Block::Named $)` goes
# on to the parent's `Pod::Block::Named` candidate before a role's generic
# `Pod::Block:D` one: `Pod::Block::Named` is the narrower type. The deferral
# ranking scored a type by its MRO length floored at 3, and the internal MRO of
# the parentless `Pod::Block` (and so of `Pod::Block::Named`) lacks the
# `Any, Mu` tail, so both scored 3 and the role's candidate won (Pod::TreeWalker
# t/no-descend).

plan 4;

role Listener {
    multi method start(Pod::Block:D $node) { 'role' }
}
class Base does Listener {
    multi method start(Pod::Block::Code $node)  { 'base-code' }
    multi method start(Pod::Block::Named $node) { 'base-named' }
}
my class Kid is Base {
    multi method start(Pod::Block::Named $node) { 'kid<' ~ callsame() ~ '>' }
    multi method start(Pod::Block::Para $node)  { 'kid<' ~ callsame() ~ '>' }
}

=begin pod
=IGNORE this content
=end pod

=begin pod
plain text
=end pod

my $named = $=pod[0];
is Kid.new.start($named), 'kid<base-named>', 'callsame reaches the parent\'s Named candidate';
is Base.new.start($named), 'base-named', 'the parent picks its own Named candidate';
is Kid.new.start($=pod[1].contents[0]), 'kid<role>',
    'a type only the role handles defers to the role candidate';

role R { }
subset SR of R;
class Catch {
    multi method m($a)      { 'any' }
    multi method m(SR $a)   { 'role:' ~ callsame() }
}
is Catch.m(class :: does R {}.new), 'role:any', 'a role constraint still ranks above Any';
