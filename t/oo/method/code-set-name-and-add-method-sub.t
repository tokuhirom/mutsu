use Test;

# #9479 (PDF::COS::Tie's COSAttrHOW.compose):
#
#     my &accessor = sub (\obj) is rw { obj.rw-accessor(self, :$key) };
#     &accessor.set_name($key);
#     $package.^add_method($key, &accessor);
#
# `Code.set_name` fell through to method composition (answering a
# `<composed-method:set_name>` Sub, name unchanged), and a `sub` handed to
# `^add_method` never received the invocant as its first positional.

plan 10;

# --- set_name renames the code object itself --------------------------------
{
    sub rename-inside { my &acc = sub ($x) { 42 }; &acc.set_name("foo"); &acc.name }
    is rename-inside(), 'foo', 'set_name inside a routine';

    my &b = sub { 1 };
    &b.set_name("bar");
    my &c = &b;
    is &b.name, 'bar', 'set_name at mainline';
    is &c.name, 'bar', 'every alias sees the new name';
    is b(), 1, 'the renamed code still runs';

    my $m = method { 1 };
    $m.set_name("mm");
    is $m.name, 'mm', 'a method literal can be renamed too';
}

# --- a sub passed to ^add_method takes the invocant positionally -------------
{
    class D { has $.v = 3 }
    D.^add_method("hi", sub ($o) { "hi {$o.^name}" });
    is D.new.hi, 'hi D', 'sub ($o): the invocant is the first positional';
    D.^add_method("two", sub ($o, $x) { "two {$o.v} $x" });
    is D.new.two(9), 'two 3 9', 'and the remaining arguments follow it';
    D.^add_method("m", method ($a) { "m {self.v} $a" });
    is D.new.m(1), 'm 3 1', 'a method literal still has an implicit invocant';
}

# --- the COSAttrHOW shape end to end -----------------------------------------
{
    my role AttrHOW {
        method compose(Mu $package) {
            my $key = self.name.substr(2);
            my &accessor = sub (\obj) is rw { obj.get($key) };
            &accessor.set_name($key);
            try $package.^add_method($key, &accessor);
            callsame;
        }
    }
    multi trait_mod:<is>(Attribute $att, :$entry!) { $att does AttrHOW }
    class C {
        has $.x is entry;
        method get($k) { "got $k" }
    }
    is C.new.x, 'got x', 'an attribute HOW installs a renamed sub as the accessor';
    is C.^find_method("x").name, 'x', 'which reports its set_name';
}
