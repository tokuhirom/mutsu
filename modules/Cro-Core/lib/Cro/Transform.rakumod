use Cro::Message;

role Cro::Transform {
    method consumes() { ... }
    method produces() { ... }
    method transformer(Supply:D $pipeline) returns Supply:D { ... }
}
