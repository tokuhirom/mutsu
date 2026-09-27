unit role ParamRoleExportedEnum[::KeyT];

# A `my enum ... is export` declared in a *parameterized* unit role: the enum
# is a compile-time declaration, so it must be importable (and be the same
# type the role's attributes are typed with) before any composition.
my enum TOrder is export <DESC ASC>;

has TOrder $!order-by;

submethod BUILD(TOrder :$!order-by) {
    $!order-by = TOrder::ASC unless $!order-by.defined;
}

method order-by { $!order-by }
