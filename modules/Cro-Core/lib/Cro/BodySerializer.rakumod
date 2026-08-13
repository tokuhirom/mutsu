use Cro::MessageWithBody;

role Cro::BodySerializer {
    method is-applicable(Cro::MessageWithBody $message, $body --> Bool) { ... }
    method serialize(Cro::MessageWithBody $message, $body --> Supply) { ... }
}
