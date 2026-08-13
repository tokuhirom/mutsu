use Cro::MessageWithBody;

role Cro::BodyParser {
    method is-applicable(Cro::MessageWithBody $message --> Bool) { ... }
    method parse(Cro::MessageWithBody $message --> Promise) { ... }
}


