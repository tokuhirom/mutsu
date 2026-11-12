unit module ImportedProtoFamilyMemo;

my class NotFound is Int { }

my proto sub locate(|) is export {*}
my multi sub locate(@a, $needle) {
    for @a.kv -> $i, $v { return $i if $v == $needle }
    NotFound.new(@a.elems)
}
my multi sub locate(Str:D $s, Str:D $needle) { $s.index($needle) // NotFound.new($s.chars) }

sub found($r) is export { $r !~~ NotFound }
