class X::Cro::MediaType::Invalid is Exception {
    has Str $.media-type;
    method message() { "Could not parse media type '$!media-type'" }
}

#| Provides for parsing and serialization of media types
class Cro::MediaType {
    #| Given the example "application/vnd.foo+json; charset=UTF-8", this
    #| would return "application"
    has Str $.type is required;

    #| Given the example "application/vnd.foo+json; charset=UTF-8", this
    #| would return "json"
    has Str $.suffix = '';

    #| Given the example "application/vnd.foo+json; charset=UTF-8", this
    #| would return "foo"
    has Str $.subtype-name is required;

    #| Given the example "application/vnd.foo+json; charset=UTF-8", this
    #| would return "vnd"
    has Str $.tree = '';

    #| Given the example "application/vnd.foo+json; charset=UTF-8", this
    #| would return an array containing a Pair with key "charset" and
    #| value "UTF-8"
    has Pair @.parameters;

    grammar Grammar {
        token TOP { <type> '/' <subtype> <parameters> \s* ';'? }
        token type { <[A..Za..z0..9_-]>+ }
        token restricted-name { <[A..Za..z0..9]> <[A..Za..z0..9!#$&^_-]>* }
        token subtype {
            <head=.restricted-name> ['.' <sub=.restricted-name>]*
            ['+' <suffix=.restricted-name>]*
        }
        token parameters {
            [\s* ';' \s* <parameter>]*
        }
        token parameter {
            <attribute=.token> '=' <value>
        }
        proto token value {*}
        token value:sym<token> {
            <token>
        }
        token value:sym<quoted-string> {
            '"' ~ '"' [<qtext> | <quoted-pair>]*
        }
        token qtext { <-["\\\n]>+ }
        token quoted-pair { \\ <( . )> }
        token token { <[A..Za..z0..9!#$%&'*+^_`{|}~\.-]>+ }
    }

    class Actions {
        method TOP($/) {
            make Cro::MediaType.bless(
                type => ~$<type>,
                |$<subtype>.ast,
                parameters => $<parameters>.ast
            );
        }
        method subtype($/) {
            my %parts;
            if +$<sub> {
                %parts<tree> = ~$<head>;
                %parts<subtype-name> = join '.', $<sub>;
            } else {
                %parts<subtype-name> = ~$<head>;
            }
            if +$<suffix> {
                my @extra-parts = $<suffix>>>.Str;
                %parts<suffix> = @extra-parts.pop;
                %parts<subtype-name> ~= join '', ('+' X~ @extra-parts);
            }
            make %parts;
        }
        method parameters($/) {
            make $<parameter>.map(*.ast);
        }
        method parameter($/) {
            make ~$<attribute> => $<value>.ast;
        }
        method value:sym<token>($/) {
            make ~$/;
        }
        method value:sym<quoted-string>($/) {
            make $/.caps.map(*.value.Str).join;
        }
    }

    #| Parse a media type, such as text/html, into a Cro::MediaType object
    method parse(Str() $media-type --> Cro::MediaType) {
        with Grammar.parse($media-type, :actions(Actions)) {
            .ast
        }
        else {
            die X::Cro::MediaType::Invalid.new(:$media-type)
        }
    }

    #| Given the example "application/vnd.foo+json; charset=UTF-8", this
    #| would return "vnd.foo+json"
    method subtype() {
        ($!tree ?? "$!tree." !! "") ~
            $!subtype-name ~
            ($!suffix ?? "+$!suffix" !! "")
    }

    #| Given the example "application/vnd.foo+json; charset=UTF-8", this
    #| would return "application/vnd.foo+json"
    method type-and-subtype() {
        "$!type/$.subtype"
    }

    #| Transform the Cro::MediaType object into a string representation of
    #| the media type, for example for use in a HTTP Content-type header
    multi method Str(Cro::MediaType:D:) {
        "$!type/$.subtype" ~ @!parameters.map(&param-str).join
    }

    sub param-str(Pair $p) {
        my $prefix = "; $p.key()=";
        my $value = $p.value;
        Grammar.parse($value, :rule<token>)
            ?? "$prefix$value"
            !! qq[$prefix"$value.subst('"', '\\"', :g)"]
    }
}
