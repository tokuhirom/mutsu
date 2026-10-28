unit module MultiOnlyExport;
multi shout(Str $a, Str $b --> Str) is export { "two:$a$b" }
multi shout(Str $a --> Str) is export { "one:$a" }
