use Cro::BodySerializer;
use Cro::MessageWithBody;

class X::Cro::BodySerializerSelector::NoneApplicable is Exception {
    has Str $.hint;
    has Str $.body-type;

    submethod BUILD(Cro::MessageWithBody :$message!, :$response-body!) {
        with $message {
            $!hint = $message.error-hint;
        }
        with $response-body {
            $!body-type = $response-body.^name;
        }
    }

    method message() {
        "No applicable body serializer could be found for this message" ~
          ($!hint ?? "\n$!hint" !! "") ~
          ($!body-type ?? ", with a body of type $!body-type" !! "");
    }
}

role Cro::BodySerializerSelector {
    method select(Cro::MessageWithBody $body --> Cro::BodySerializer) { ... }
}

class Cro::BodySerializerSelector::List does Cro::BodySerializerSelector {
    has Cro::BodySerializer @.serializers;

    method select(Cro::MessageWithBody $message, $body --> Cro::BodySerializer) {
        for @!serializers {
            .return if .is-applicable($message, $body);
        }
        die X::Cro::BodySerializerSelector::NoneApplicable.new(:$message, :response-body($body));
    }
}

class Cro::BodySerializerSelector::Prepend does Cro::BodySerializerSelector {
    has Cro::BodySerializer @.serializers;
    has Cro::BodySerializerSelector $.next is required;

    method select(Cro::MessageWithBody $message, $body --> Cro::BodySerializer) {
        for @!serializers {
            .return if .is-applicable($message, $body);
        }
        $!next.select($message, $body);
    }
}
