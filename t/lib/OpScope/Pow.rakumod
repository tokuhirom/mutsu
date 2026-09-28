unit module OpScope::Pow;
multi infix:<**>(Int:D $a where * == 2, Int:D $b) is export { "custom" }
