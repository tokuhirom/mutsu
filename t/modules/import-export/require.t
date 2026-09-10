use Test;
use lib 't/lib';

plan 10;

# Basic require + class instantiation
{
    require RequireGreeter;
    my $g = RequireGreeter.new(name => "World");
    is $g.greet, "Hello, World!", "require Module; Module.new(...) works";
}

# Attribute access on required class
{
    require RequireGreeter;
    my $g = RequireGreeter.new(name => "Attr");
    is $g.name, "Attr", "attribute accessor works on required class";
}

# Require with symbol import
{
    require RequireFuncs <&require-add>;
    is require-add(2, 3), 5, "require with symbol import works";
}

# Indirect name lookup with ::()
{
    require RequireGreeter;
    my $g = ::("RequireGreeter").new(name => "Indirect");
    is $g.greet, "Hello, Indirect!", '::("Module").new works after require';
}

# Dynamic require with ::($var)
{
    my $mod = "RequireGreeter";
    require ::($mod);
    my $g = ::("RequireGreeter").new(name => "Dynamic");
    is $g.greet, "Hello, Dynamic!", "dynamic require ::(\$var) works";
}

# Require inside a conditional block
{
    if True {
        require RequireGreeter;
        my $g = RequireGreeter.new(name => "Block");
        is $g.greet, "Hello, Block!", "require inside block works";
    }
}

# Double require does not error
{
    require RequireGreeter;
    require RequireGreeter;
    my $g = RequireGreeter.new(name => "Twice");
    is $g.greet, "Hello, Twice!", "requiring the same module twice does not error";
}

# require returns a type object
{
    my $result = (require RequireGreeter);
    is $result.gist, "(RequireGreeter)", "require returns the type object";
}

# Class method on required module
{
    require RequireGreeter;
    my $g = RequireGreeter.new(name => "Method");
    ok $g.greet.starts-with("Hello"), "method call on required class works";
}

# Required class isa check
{
    require RequireGreeter;
    my $g = RequireGreeter.new(name => "Isa");
    isa-ok $g, RequireGreeter, "isa-ok works for required class";
}
