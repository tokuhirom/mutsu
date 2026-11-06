use v6;
use Test;

# `::<M>` is a lookup through the enclosing lexical scopes, so a routine sees
# the packages of its file even though its own frame holds none of them.
# Found with BinaryHeap (`sub EXPORT { my %e = ::<BinaryHeap>.WHO; ... }`).
# Identical under raku.

plan 5;

module M { class C is export {}; our sub f() { 1 } }

sub names() { ::<M>.WHO.keys.sort.join(",") }
sub nm() { ::<M>.^name }
sub copy() { my %x = ::<M>.WHO; %x.keys.sort.join(",") }

is ::<M>.^name, "M", 'at file level';
is nm(), "M", 'inside a routine';
is names(), "&f,C,EXPORT", 'its stash keys inside a routine';
is copy(), "&f,C,EXPORT", 'copied into a hash inside a routine';
ok ::<M>.WHO<C>:exists, 'a member of the stash';
