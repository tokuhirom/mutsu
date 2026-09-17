use Test;

# `.^can('name')` on an auto-generated attribute accessor built a `Method`
# `Instance` via `Value::routine_parts` (methods_classhow_method_obj.rs,
# `collect_can_methods`), a compact (package, name) handle carrying no
# `is_rw` info at all — unlike `.^find_method`/`.^lookup`, which build a full
# `Method` `Instance` with an `rw` attribute (`wrap_accessor_method_object`).
# `dispatch_routine_method` (methods_sub.rs) had no "rw"/"readonly" case for
# that handle shape, so `.rw` died with "No such method 'rw' for invocant of
# type 'Method'" even though `.WHAT` correctly reported `(Method)`.
#
# Found via the `Object::Permission` ecosystem distribution: its custom
# `is authorised-by(...)` trait fetches the target method with
# `$package.^can($name)[0]` (PermissionedAttribute.compose) and immediately
# calls `.rw` on it to decide which wrapper closure to install.

class Foo {
    has $.ro-attr;
    has $.rw-attr is rw;
}

my $ro = Foo.^can('ro-attr')[0];
is $ro.WHAT.^name, 'Method', 'can() on a readonly accessor returns a Method';
nok $ro.rw, 'readonly accessor: .rw is False';

my $rw = Foo.^can('rw-attr')[0];
is $rw.WHAT.^name, 'Method', 'can() on an rw accessor returns a Method';
ok $rw.rw, 'rw accessor: .rw is True';

# Same accessors reached via .^find_method agree with .^can, so the two
# lookup paths stay in lockstep.
is Foo.^find_method('ro-attr').rw, $ro.rw, 'find_method agrees with can on readonly';
is Foo.^find_method('rw-attr').rw, $rw.rw, 'find_method agrees with can on rw';

done-testing;
