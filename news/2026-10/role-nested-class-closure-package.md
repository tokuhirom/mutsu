# A closure in a method of a class declared inside a role body belongs to that class

`lexical_closure_package_sym` gave every closure made in a method whose `lexical_package` is a role
that role's package, so a class declared inside the role body (`role R { my class Config { sub helper
{...}; method m { (1,2).map({ helper($_) }) } } }`) lost its own file-scoped subs inside the closure:
`Unknown function: helper`. A method whose owner is nested under the role keeps its own class now;
a method composed into another class still takes the role. Pakku's `config` suite passes (mutsu#14870).
