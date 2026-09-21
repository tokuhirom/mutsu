use Test;

plan 1;

# Selecting a multi method candidate whose OWN `where` clause dispatches a
# nested method call (`.HOW`/`~~`) must not disturb the caller-variable
# bookkeeping (`pending_call_arg_sources`) an UNRELATED sibling candidate's
# `is rw` positional parameter needs at bind time. `method_args_match_for_
# invocant` already rolls back `env`/`current_package` around this
# speculative window; `pending_call_arg_sources` was missing from that
# rollback, so evaluating one candidate's `where` guard silently cleared it
# for the candidate ultimately selected — an `is rw` Buf parameter bound from
# a real lexical then failed with "expects a writable container" only
# because a SIBLING candidate happened to carry a `where` clause (ASN::BER's
# `ASN::Parser.parse` dispatch: an `enum-type where $enum-type.HOW ~~
# Metamodel::EnumHOW` candidate cleared the sources meant for the
# `ASNSequence $type is rw` candidate actually chosen).

class Rocket {}

class Parser {
    has $.type;

    multi method parse(Buf $input, :$to-chop = True) {
        my $in = Buf.new($input);
        self.parse($in, $!type);
    }
    multi method parse(Buf $input is rw, Rocket $type) {
        "parsed {$input.elems} bytes";
    }
    # A sibling candidate whose `where` clause runs a nested method dispatch
    # (`.HOW ~~ ...`) during speculative matching.
    multi method parse(Buf $input is rw, $enum-type where $enum-type.HOW ~~ Metamodel::EnumHOW) {
        "enum branch unreachable for this call";
    }
}

is Parser.new(type => Rocket.new).parse(Buf.new(1, 2, 3)), 'parsed 3 bytes',
    'an is-rw candidate still binds a real lexical after a sibling where-clause runs';

# vim: expandtab shiftwidth=4
