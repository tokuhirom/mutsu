unit module CapturedStageProgram;

my class NotFound is Int { }
our sub notfound() is export { NotFound.new(1) }

my proto sub put-in(|) is export {*}
my multi sub put-in(\h, $n, NotFound :$pos!)       { "pos" }
my multi sub put-in(\h, $n, :&cmp, :$force)        { "cmp-force:" ~ ($force // 'no') }
my multi sub put-in(\h, $n, **@also, :$force)      { "also(" ~ @also.elems ~ "):" ~ ($force // 'no') }
my multi sub put-in(Int \h, Int:D $n, :$force)     { "int:" ~ ($force // 'no') }
my multi sub put-in(Str \h, Str:D $n)              { "str" }
my multi sub put-in(@a where *.elems > 2, $n)      { "long-array" }
my multi sub put-in(@a, $n)                        { "array" }
