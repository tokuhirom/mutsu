use Test;

# A bare type object passed to an implicit sigilless/raw parameter is a value,
# not a writable caller variable. This is the nested shape used by
# JSON::Unmarshal's `maybe-nominalize(type)` helper.
plan 2;

class Outer { }
class Inner { }

sub maybe-nominalize(Mu \obj) is raw {
    obj.HOW.archetypes.nominalizable ?? obj.^nominalize !! obj
}

sub inner() {
    my \type = Inner;
    maybe-nominalize(type);
    type
}

sub outer() {
    my \type = Outer;
    inner();
    type
}

is outer(), Outer, 'nested type-object calls do not write back to the caller';

class Media { has Str $.media_type }
class Person { has @.known }
class CustomUnmarshaller {
    method unmarshal(@media) {
        custom(@media)
    }
}
my $custom-unmarshaller = CustomUnmarshaller.new;

multi decode(%json, Mu $obj is raw) {
    my \type = $obj;
    my %args;
    %args<known> = type =:= Person
        ?? $custom-unmarshaller.unmarshal([{:media_type('movie')}])
        !! [];
    type.new(|%args)
}

multi decode(@json, @x) {
    my @ret := Array[@x.of].new;
    for @json.list -> $value {
        my $type = @x.of =:= Any ?? $value.WHAT !! @x.of;
        @ret.append(decode($value, $type));
    }
    @ret
}

multi custom(@media --> Array) {
    my @out;
    for @media -> $media {
        @out.push: decode({ :media_type('movie') }, Media)
    }
    Array.new(|@out)
}

my $people = decode([{:known([{:media_type('movie')}])}], Array[Person]);
is $people[0].^name,
    'Person',
    'typed array decoding preserves the outer type across nested custom decoding';
