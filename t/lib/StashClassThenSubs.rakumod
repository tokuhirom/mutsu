class StashClassThenSubs {
    my role Hidden { }
    method m { 1 }
}
my sub lexical-helper() is export { 3 }
sub plain-helper() is export { 4 }
my proto sub infix:<^...>(|) is export is equiv(&infix:<...>) {*}
my multi sub infix:<^...>(Mu $a, Whatever) { 'whatever:' ~ ($a ^... 9) }
my multi sub infix:<^...>(Mu $a, Mu $b) { "mu($b)" }
