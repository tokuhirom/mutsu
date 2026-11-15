use Test;

plan 13;

class MyValueObjAt is ValueObjAt {
    method Str { 'custom' }
}
my $custom = MyValueObjAt.new('q');
is $custom.Str, 'custom', 'a subclass can override Str';
is $custom.^name, 'MyValueObjAt', 'the constructor preserves the subclass';
is $custom.WHICH.^name, 'MyValueObjAt', 'WHICH preserves the subclass';
is $custom.WHICH.Str, 'custom', 'WHICH keeps the subclass method override';

class PlainValueObjAt is ValueObjAt { }
my $value = PlainValueObjAt.new('text');
is $value.Str, 'text', 'a subclass inherits Str';
is $value.gist, 'text', 'a subclass inherits gist';
is $value.raku, 'PlainValueObjAt.new("text")', 'a subclass renders as its own constructor';
is $value.WHICH.Str, 'PlainValueObjAt|text', 'WHICH carries the identity text';
is $value.WHICH.WHICH.^name, 'PlainValueObjAt', 'repeated WHICH keeps the subclass';

class MyObjAt is ObjAt { }
my $object = MyObjAt.new('object');
is $object.Str, 'object', 'an ObjAt subclass inherits Str';
is $object.WHICH.^name, 'MyObjAt', 'an ObjAt subclass keeps its identity type';

class Override is ValueObjAt {
    method new($text) { "override:$text" }
}
is Override.new('x'), 'override:x', 'a declared new overrides the inherited constructor';
dies-ok { PlainValueObjAt.new() }, 'the inherited constructor requires a positional';
