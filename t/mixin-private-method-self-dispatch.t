use Test;

# A private method called on a `but role` mixin must keep `self` bound to the
# mixin wrapper, so a nested `self.foo` re-dispatches through the mixin roles
# and finds their overrides (not the inner class's base method).

plan 5;

{
    class Base {
        method plugins() { () }
        method !inner() { self.plugins }
        method via-private() { my @m = self!inner; @m.elems }
        method via-public()  { my @m = self.plugins; @m.elems }
    }
    my $o = Base.new but role :: { method plugins() { (1, 2) } };
    is $o.plugins.elems, 2, 'direct mixin override reached';
    is $o.via-public,    2, 'public method reaches mixin override via self.plugins';
    is $o.via-private,   2, 'private method reaches mixin override via self.plugins';
}

{
    # The private method itself grep/filters through the overridden method.
    role Builder { }
    class Repo does Builder {
        method plugins() { () }
        method !matching() { self.plugins.grep(*.so) }
        method run() { my @m = self!matching; @m.elems }
    }
    my $o = Repo.new but role :: { method plugins() { (1, 0, 2) } };
    is $o.run, 2, 'private method + grep sees mixin plugins';
}

{
    # Owner-qualified private call (self!Class::method) on a mixin.
    class C {
        method who() { 'BASE' }
        method !ask() { self.who }
        method call() { self!C::ask }
    }
    my $o = C.new but role :: { method who() { 'OVERRIDE' } };
    is $o.call, 'OVERRIDE', 'owner-qualified private call reaches mixin override';
}
