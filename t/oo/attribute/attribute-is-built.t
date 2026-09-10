use Test;

plan 4;

# Attribute.is_built: true for public attrs, false for `has $!x` privates,
# overridable by the `is built` trait (JSON::Marshal skips private attributes
# via `.has_accessor || .is_built`).
class C {
    has Str $.pub;
    has Str $!priv = 'p';
    has Int $!half is built = 42;
}

my %by-name = C.^attributes(:local).map({ .name => $_ });
ok %by-name<$!pub>.is_built, 'public attribute is_built';
nok %by-name<$!priv>.is_built, 'plain private attribute is not is_built';
ok %by-name<$!half>.is_built, 'is built trait makes a private attribute built';
ok %by-name<$!pub>.has_accessor, 'has_accessor still true for public';

done-testing;
