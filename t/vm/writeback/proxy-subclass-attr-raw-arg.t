use Test;
use nqp;

plan 4;

# A `$!val` attribute of a Proxy subclass, bound to a Scalar, is passed to an
# `is raw` parameter as that Scalar: `Attribute.set_value` then binds the very
# container (AttrX::Mooish's AttrProxy / `unbind-proxy`).
class Foo { has $.a is rw = 1; }
Foo.^add_method('nick', Foo.^method_table<a>);
my $o = Foo.new;
my $attr = Foo.^attributes[0];

class Binder {
    has $.attr;
    method unbind(Mu $obj is raw, Mu $v is raw) { $!attr.set_value($obj, $v) }
}

class Cell is Proxy {
    has Mu $.val;
    has $.binder;
    has $.inst;
    method new(:$FETCH, :$STORE, *%init) is raw { callwith(:$FETCH, :$STORE).VAR!SET(%init) }
    method !SET(%init) is raw { $!binder := %init<binder>; $!inst := %init<inst>; self }
    method store-value(Mu $value is raw) is raw {
        $!val := nqp::clone_nd($attr.auto_viv_container);
        $!val = $value;
        $!binder.unbind($!inst, $!val);
        $!val
    }
}

my $cell := Cell.new(:FETCH(-> $ { 1 }), :STORE(-> $, $ { }), :binder(Binder.new(:$attr)), :inst($o));
$cell.VAR.store-value(5);
is $o.a, 5, 'the value was stored';
$o.a = 6;
is $cell.VAR.val, 6, 'assigning through the accessor reaches the Proxy attribute cell';
$o.nick = 7;
is $o.a, 7, 'an accessor alias writes the shared container';
is $cell.VAR.val, 7, 'and the Proxy attribute sees it';
