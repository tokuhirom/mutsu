class SinkFindMethodLib::Bar {
    method ^find_method(Mu \type, Str:D $name) {
        return my method is-generic(--> False) { } if $name eq 'is-generic';
        my method h(|c) { @*SEEN.push("h:$name") }
        &h
    }
}

my sub trampoline(&code) is raw {
    use nqp;
    nqp::create(SinkFindMethodLib::Bar)
}

my %EXPORT;
module SinkFindMethodLib {
    BEGIN {
        %EXPORT<&slack> := &trampoline;
    }
}

sub EXPORT { %EXPORT }
