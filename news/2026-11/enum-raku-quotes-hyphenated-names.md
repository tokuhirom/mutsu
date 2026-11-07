# `.raku` of an enum value quotes every name that is not made of word characters

mutsu printed `Qos::At-least-once` for an enum value whose name has a hyphen;
rakudo prints `Qos::<At-least-once>` (and the same for a quote, a dot or a space
in the name), leaving the name bare only when every character is a word
character. Seen in Protocol::MQTT's test output (#16992).
