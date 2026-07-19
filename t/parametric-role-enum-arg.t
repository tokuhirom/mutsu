use v6;
use Test;

# A parametric role whose type parameter is constrained by an enum type, applied
# with an enum *value* as the argument, must bind the value (not a Package type
# object). Previously the role-argument evaluator treated any `Foo::Bar` name as
# a type expression, so `does Packet[Type::Connect]` bound a Package and the
# candidate failed with "No matching candidate found for the parametric role".
# Surfaced by SQL::Abstract and Protocol::MQTT.

plan 5;

enum Type <Connect Publish Subscribe>;

role Packet[Type $type] {
    method kind { $type }
}

class Conn does Packet[Type::Connect] {}
class Pub  does Packet[Type::Publish] {}

is Conn.new.kind, Connect, 'enum-value role arg binds (Connect)';
is Pub.new.kind,  Publish, 'enum-value role arg binds (Publish)';
isa-ok Conn.new.kind, Type, 'the bound value is a Type enum value';

# A string / literal type argument still works (unchanged behavior).
role Named[Str $name] { method name { $name } }
class Widget does Named["widget"] {}
is Widget.new.name, "widget", 'string role arg still binds';

# A genuine type argument (a class) still binds as a type object.
class Element {}
role Container[::T] { method elem-type { T } }
class Box does Container[Element] {}
is Box.new.elem-type.^name, "Element", 'class type role arg still binds as a type';
