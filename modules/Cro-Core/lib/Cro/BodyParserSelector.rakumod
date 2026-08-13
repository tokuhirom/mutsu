use Cro::BodyParser;
use Cro::MessageWithBody;

class X::Cro::BodyParserSelector::NoneApplicable is Exception {
    method message() {
        "No applicable body parser could be found for this message"
    }
}

role Cro::BodyParserSelector {
    method select(Cro::MessageWithBody --> Cro::BodyParser) { ... }
}

class Cro::BodyParserSelector::List does Cro::BodyParserSelector {
    has Cro::BodyParser @.parsers;

    method select(Cro::MessageWithBody $message --> Cro::BodyParser) {
        for @!parsers {
            .return if .is-applicable($message);
        }
        die X::Cro::BodyParserSelector::NoneApplicable.new;
    }
}

class Cro::BodyParserSelector::Prepend does Cro::BodyParserSelector {
    has Cro::BodyParser @.parsers;
    has Cro::BodyParserSelector $.next is required;

    method select(Cro::MessageWithBody $message --> Cro::BodyParser) {
        for @!parsers {
            .return if .is-applicable($message);
        }
        $!next.select($message);
    }
}
