use Test;

# A type minted by `Metamodel::*HOW.new_type(:name(X))` under a name another
# type already holds is a DIFFERENT type that merely shares a display name
# (`X\0<id>` internally). Its `.WHO` stash members must stay its own: binding
# one must not clobber the same-named member of the type that already holds the
# name, and vice versa.
#
# Came from Red's t/104: `MetamodelX::Red::Specialisable` mints a projection
# model named after the source model and binds `type.WHO<ResultSeq> :=
# Proxy.new(...)` on it; the declared model's own `ResultSeq` was overwritten
# because the minted type's mangled storage name mirrored its members under the
# de-mangled facing name into the shared package store (#17481).

plan 4;

my \TypeA = Metamodel::ClassHOW.new_type(:name<Shared>);
TypeA.^compose;
TypeA.WHO<Member> := 'original';
my $orig = TypeA.WHO<Member>;
is $orig, 'original', 'the first type records its stash member';

my \TypeB = Metamodel::ClassHOW.new_type(:name<Shared>);
TypeB.^compose;
TypeB.WHO<Member> := 'other';

is TypeB.WHO<Member>, 'other', 'the second same-named type records its own member';
is TypeA.WHO<Member>, 'original',
    'the first type keeps its member after the second binds the same key';

# A member bound only on the second type does not appear on the first.
TypeB.WHO<Only> := 'b-only';
nok TypeA.WHO<Only>:exists, 'a member of the second type is not a member of the first';
