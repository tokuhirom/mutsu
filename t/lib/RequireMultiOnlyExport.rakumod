unit module RequireMultiOnlyExport;
multi crypt(Str $a, Str $b --> Str) is export { "two" }
multi crypt(Str $a --> Str) is export { "one" }
sub plain-one() is export { "plain" }
