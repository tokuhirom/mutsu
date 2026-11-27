unit module Mzef::E2E::Fixture;

sub fixture-greeting(Str $name --> Str) is export {
    "hello, $name"
}
