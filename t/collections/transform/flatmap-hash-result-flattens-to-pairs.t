use Test;

# `.flatmap` is `.map(...).flat`: a Hash the mapper returns flattens to its
# pairs, while an itemized one stays a single element. Red's
# `Red::AST::AND.should-set` is `[$.left, $.right].flatmap({ .should-set })`
# under `--> Hash()`; the second Hash was left whole, so
# `.create` on a two-condition `grep` lost its column values (Red 0.2.7
# t/51-pre-create-or-update).

plan 6;

my @itemized = ({a => 1}, {b => 2});

is [{a => 1}, {b => 2}].flatmap({ .self }).raku, '(:a(1), :b(2)).Seq',
    'bare Hash results flatten to pairs';
is @itemized.flatmap({ $_ }).elems, 2, 'itemized Hash results stay single';

sub merged(--> Hash()) { [{a => 1}, {b => 2}].flatmap({ .self }) }
is merged().raku, '{:a(1), :b(2)}', 'a Hash() return type collects the flattened pairs';

class Cond {
    has %.set;
    method should-set(--> Hash()) { %!set }
}
sub both(--> Hash()) {
    [Cond.new(set => {guild => 1}), Cond.new(set => {user => 2})].flatmap({ .should-set })
}
is both().raku, '{:guild(1), :user(2)}', 'a method returning a Hash, flatmapped';

is (1, 2).flatmap({ {n => $_} }).elems, 2, 'a Hash built per element yields its pair each';
is ({a => 1},).flatmap({ .self }).elems, 1, 'a single-pair Hash contributes one pair';
