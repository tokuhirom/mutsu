use Test;

# `.bless` seeds an attribute that has no initializer with its declared type
# object, exactly as `.new` does. The seed default is evaluated once per class
# and remembered, so a repeated bless must keep producing the right seeds, and
# `is required` must still be enforced after BUILD/TWEAK on every bless.

plan 11;

role R {
    has Str $.scheme;
    has Int $.port;
    has $.untyped;
}

class U does R {
    has Str $.path;
}

for ^3 {
    my $u = U.bless(path => "/p$_");
    is $u.path, "/p$_", "bless sets a supplied attribute (round $_)";
    ok $u.scheme === Str && $u.port === Int && $u.untyped === Any,
        "unsupplied attributes hold their type objects (round $_)";
}

my $n = U.new(path => '/n');
ok $n.scheme === Str, '.new after .bless still seeds the declared type object';
is U.bless(scheme => 'https').scheme, 'https', 'a supplied typed attribute wins over its seed';

class Req {
    has Str $.name is required;
    has Int $.n;
}

throws-like { Req.bless(n => 1) }, X::Attribute::Required,
    message => /'$!name' .* 'is required'/,
    'a missing required attribute dies through bless';
throws-like { Req.bless(n => 2) }, X::Attribute::Required,
    message => /'$!name' .* 'is required'/,
    '... and again on a repeated bless of the same class';
is Req.bless(name => 'x').name, 'x', 'a supplied required attribute binds';
