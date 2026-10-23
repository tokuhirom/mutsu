use v6;
use Test;
use nqp;

plan 4;

# An attribute bound to a typed container (here a clone of another attribute's
# `auto_viv_container`) stores into that container, so the container's own
# `:D` constraint applies, not the bare `Mu` the holder attribute declares.
class Foo { has Str:D $.bar is rw = "" }
class Holder {
    has Mu $!val;
    method init(Mu $c is raw) { $!val := nqp::clone_nd($c) }
    method store(Mu $value is raw) { $!val = $value }
    method rebind(Mu $c is raw) { $!val := nqp::clone_nd($c) }
}
my $attr = Foo.^attributes.first(*.name eq '$!bar');
my $h = Holder.new;
$h.init($attr.auto_viv_container);
throws-like { $h.store(5) },   X::TypeCheck::Assignment, 'a wrong type is refused';
throws-like { $h.store(Nil) }, X::TypeCheck::Assignment, 'Nil is refused by a :D container';
lives-ok { $h.store("fine") }, 'a matching value is accepted';
lives-ok { $h.rebind($attr.auto_viv_container) }, 'rebinding the attribute is not type-checked';

done-testing;
