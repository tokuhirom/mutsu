use Test;

plan 4;

# A class declared with a compound name at file scope (`class Foo::Bar`) is
# declared in GLOBAL, not inside `Foo`: a package-qualified type name used in
# its body resolves from GLOBAL, never by re-deriving `Foo` as a scope.
class X::Baz { method who { 'global' } }
class Foo::X::Baz { method who { 'foo' } }
class Foo::Bar {
    method who { X::Baz.who }
    method name { X::Baz.^name }
}
is Foo::Bar.who, 'global', 'X::Baz inside Foo::Bar is the GLOBAL class';
is Foo::Bar.name, 'X::Baz', '... and names it so';

# The LLM::Chat shape: a wrapper class whose name ends in the wrapped class's
# name holds an attribute of the wrapped (GLOBAL) type.
class Template::Jinja2 { method who { 'wrapped' } }
class LLM::Chat::Template { }
class LLM::Chat::Template::Jinja2 is LLM::Chat::Template {
    has Template::Jinja2 $!env;
    method render { $!env = Template::Jinja2.new; $!env.who }
}
is LLM::Chat::Template::Jinja2.new.render, 'wrapped',
    'Template::Jinja2 inside LLM::Chat::Template::Jinja2 is not the class itself';

# Real nesting still resolves through the enclosing package.
module NL {
    class Inner::Thing { method who { 'nested' } }
    class User { method who { Inner::Thing.who } }
}
is NL::User.who, 'nested', 'a qualified name still resolves inside a real enclosing package';
