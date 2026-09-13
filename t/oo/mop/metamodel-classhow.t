use Test;
plan 9;

# Metamodel::ClassHOW.new_type creates a usable type
{
    my $type = Metamodel::ClassHOW.new_type(name => "DynClass");
    ok $type.defined.not, 'new_type returns a type object (not defined)';
    is $type.^name, 'DynClass', 'new_type name is correct';
}

# compose and new work on dynamically created types
{
    my $type = Metamodel::ClassHOW.new_type(name => "Composed");
    $type.^compose;
    my $obj = $type.new;
    ok $obj.defined, 'can instantiate dynamically created type after compose';
    ok $obj.WHAT ~~ $type, 'instance smartmatches against its type';
}

# add_attribute works on dynamically created types
{
    my $inner = Metamodel::ClassHOW.new_type(name => 'AttrInner');
    $inner.^compose;

    my $outer = Metamodel::ClassHOW.new_type(name => 'AttrOuter');
    my $attr = Attribute.new(name => '$!val', type => $inner, package => $outer, :has_accessor);
    $outer.^add_attribute($attr);
    $outer.^compose;

    lives-ok { $outer.new(val => $inner.new) }, 'can construct with dynamic attribute';
}

# Metamodel::ClassHOW can be used as parent class
{
    class MyHOW is Metamodel::ClassHOW {
        method custom-method { 42 }
    }
    ok MyHOW.^isa(Metamodel::ClassHOW), 'can inherit from Metamodel::ClassHOW';
}

# Attribute.new with :has_accessor creates accessor
{
    my $type = Metamodel::ClassHOW.new_type(name => 'WithAccessor');
    my $attr = Attribute.new(name => '$!greeting', type => Str, package => $type, :has_accessor);
    $type.^add_attribute($attr);
    $type.^compose;
    my $obj = $type.new(greeting => "hello");
    is $obj.greeting, 'hello', 'accessor works on dynamically added attribute';
}

# `add_role` composes a role into a dynamically-created class.
{
    role DynamicMopRole {
        method dynamic-value { 42 }
    }
    my $type = Metamodel::ClassHOW.new_type(name => 'WithDynamicRole');
    $type.^add_role(DynamicMopRole);
    $type.^compose;
    is $type.new.dynamic-value, 42, 'dynamically added role method is available';
}

# The core `is rw` trait marks an Attribute before ClassHOW consumes it.
{
    my $type = Metamodel::ClassHOW.new_type(name => 'WithDynamicRw');
    my $attr = Attribute.new(name => '$!value', type => Int, package => $type, :has_accessor);
    trait_mod:<is>($attr, :rw);
    $type.^add_attribute($attr);
    $type.^compose;
    my $obj = $type.new(value => 1);
    $obj.value = 2;
    is $obj.value, 2, 'dynamically added rw attribute has a writable accessor';
}
