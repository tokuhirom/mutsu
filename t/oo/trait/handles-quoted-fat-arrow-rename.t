use v6.d;
use Test;

plan 1;

class Delegate {
    method value() { 42 }
}

class Wrapper {
    has Delegate $.delegate handles ("alias" => "value");
}

is Wrapper.new(delegate => Delegate.new).alias, 42,
    'quoted exposed and target names work in an attribute handles rename';
