use lib 't/lib';
use Test;

plan 3;

# LLM::Agent (ecosystem): `Str:D :$id = uuid-v4 --> Str:D` with `uuid-v4`
# imported from UUID::V4. An imported routine is a listop, and `-->` was read
# as the start of its argument, so the whole `method` failed to parse.
use ImportedNullaryDefault;

sub with-named(Str:D :$id = fresh-id --> Str:D) { $id }
sub with-positional($id = bare-id --> Str) { $id }
class C {
    method !make(Str:D $type, Str:D :$id = fresh-id --> Str:D) { "$type:$id" }
    method go { self!make('t') }
}

is with-named(), 'id-1', 'named parameter default';
is with-positional(), 'id-2', 'positional parameter default';
is C.go, 't:id-1', 'private method';
