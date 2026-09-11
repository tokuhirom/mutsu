unit module A::B::C::D;

sub deep-resource-text is export {
    %?RESOURCES<hello.txt>.slurp(:close).trim
}
