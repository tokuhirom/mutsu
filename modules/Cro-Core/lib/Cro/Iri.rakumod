use Cro::Uri;
use Cro::ResourceIdentifier :decode-percents, :encode-percents;

class X::Cro::Iri::ParseError is Exception {
    has $.reason = 'malformed syntax';
    has $.iri-string is required;
    method message() {
        "Unable to parse IRI '$!iri-string': $!reason"
    }
}

class Cro::Iri does Cro::ResourceIdentifier {
    grammar GenericParser is Cro::Uri::GenericParser {
        token TOP {
            <IRI>
        }

        token IRI {
            [<scheme> || <.panic('Malformed scheme')>]
            [":" || <.panic('Missing : after scheme')>]
            <ihier-part>
            ["?" <iquery>]?
            ["#" <ifragment>]?
            [$ || <.panic('unexpected text at the end')>]
        }

        proto token ihier-part {*}
        token ihier-part:sym<authority> { "//" <iauthority> <ipath-abempty> }
        token ihier-part:sym<absolute> { <ipath-absolute> }
        token ihier-part:sym<rootless> { <ipath-rootless> }
        token ihier-part:sym<empty> { <ipath-empty> }
        token IRI-reference { [<TOP> | <irelative-ref>] }
        token absolute-IRI {
            [<scheme> || <.panic('Malformed scheme')>]
            [":" || <.panic('Missing : after scheme')>]
            <ihier-part>
            ["?" <iquery>]?
        }
        token iref {
            || <?before <.scheme> ':'> <IRI>
            || <irelative-ref>
        }
        token irelative-ref {
            <irelative-part> ["?" <iquery>]? ["#" <ifragment>]?
            [ $ || <.panic('unexpected text at the end')> ]
        }

        token irelative-part {
            | [ '//' <iauthority> <ipath-abempty> ]
            | <ipath-absolute>
            | <ipath-noscheme>
            | <ipath-empty>
        }

        token iauthority {
            [<iuserinfo> "@"]?
            <ihost> [":" <port>]?
        }

        token iuserinfo {
            [<iunreserved> | <pct-encoded> | <sub-delims> | ':']*
        }

        proto token ihost {*}
        token ihost:sym<IPv4address> {
            <.IPv4address>
        }
        regex ihost:sym<IPv6address> {
            '[' <( <.IPv6address> )> ']'
        }
        token ihost:sym<IPvFuture> {
            '[' <(
            v <[A..Fa..f0..9]>+ '.'
            <[A..Za..z0..9!$&'()*+,;=:._~-]>+
            )> ']'
        }
        token ihost:sym<ireg-name> {
            {} [<iunreserved> | <pct-encoded> | <sub-delims>]*
        }

        token ipath-abempty { ["/" <isegment>]* }
        token ipath-absolute {
            "/" [<isegment-nz> ["/" <isegment>]*]?
        }
        token ipath-noscheme {
            <isegment-nz-nc> ["/" <isegment>]*
        }
        token ipath-rootless {
            <isegment-nz> ["/" <isegment>]*
        }
        token ipath-empty { '' }

        token isegment { <ipchars>? }
        token isegment-nz { <ipchars> }
        token isegment-nz-nc { [<iunreserved> | <pct-encoded> | <sub-delims> | '@']+ }

        token ipchars { (<iunreserved>+ | <pct-encoded>+ | <sub-delims>+ | ':'+ | '@'+ | $<broken>=<[\[\]\<\>\{\}\^\"]>)+ }
        token iquery { (<ipchars> | <iprivate> | '/' | '?')* }
        token ifragment { (<ipchars> | '/' | '?')* }
        token iunreserved { [<alnum> | '-' | '.' | '_' | '~' | <ucschar>] }
        token ucschar { <[\xA0..\xD7FF     \xF900..\xFDCF   \xFDF0..\xFFEF
                          \x10000..\x1FFFD \x20000..\x2FFFD \x30000..\x3FFFD
                          \x40000..\x4FFFD \x50000..\x5FFFD \x60000..\x6FFFD
                          \x70000..\x7FFFD \x80000..\x8FFFD \x90000..\x9FFFD
                          \xA0000..\xAFFFD \xB0000..\xBFFFD \xC0000..\xCFFFD
                          \xD0000..\xDFFFD \xE1000..\xEFFFD ]> }
        token iprivate { <[\xE000..\xF8FF \xF0000..\xFFFFD \x100000..\x10FFFD]> }
        token pct-encoded {
            '%' <xdigit> <xdigit>
        }
        token sub-delims { <[!$&'()*+,;=]> }

        method panic($reason) {
            die X::Cro::Iri::ParseError.new(iri-string => self.orig, :$reason)
        }
    }

    class GenericActions is Cro::Uri::GenericActions {
        method TOP($/) {
            make $<IRI>.ast;
        }

        method IRI($/) {
            my %parts = scheme => ~$<scheme>, |$<ihier-part>.ast;
            %parts<query> = $<iquery>.ast if $<iquery>;
            %parts<fragment> = $<ifragment>.ast if $<ifragment>;
            make Cro::Iri.bless(|%parts);
        }

        method ihier-part:sym<authority>($/) {
            make {
                path => $<ipath-abempty>.ast,
                $<iauthority>.ast
            }
        }

        method ihier-part:sym<absolute>($/) {
            make (path => $<ipath-absolute>.ast);
        }

        method ihier-part:sym<rootless>($/) {
            make (path => $<ipath-rootless>.ast);
        }

        method ihier-part:sym<empty>($/) {
            make (path => $<ipath-empty>.ast);
        }

        method iauthority($/) {
            make {
                authority => ~$/,
                port => $<port> ?? $<port>.ast !! Int,
                userinfo => $<iuserinfo> ?? ~$<iuserinfo> !! Str,
                $<ihost>.ast
            };
        }

        method ihost:sym<IPv4address>($/) {
            make {
                host => ~$/,
                host-class => Cro::ResourceIdentifier::Host::IPv4
            }
        }

        method ihost:sym<IPv6address>($/) {
            make {
                host => ~$/,
                host-class => Cro::ResourceIdentifier::Host::IPv6
            }
        }

        method ihost:sym<IPvFuture>($/) {
            make {
                host => ~$/,
                host-class => Cro::ResourceIdentifier::Host::IPvFuture
            }
        }

        method ihost:sym<ireg-name>($/) {
            make {
                host => decode-percents(~$/),
                host-class => Cro::ResourceIdentifier::Host::RegName
            }
        }

        method ipath-abempty($/) {
            my $result = '';
            for @$<isegment> {
                $result ~= '/';
                $result ~= $_.ast with $_<ipchars>;
            }
            make $result;
        }

        method ipath-absolute($/) {
            my $result = '/';
            $result ~= $_<ipchars>.ast with $<isegment-nz>;
            $result ~= '/' ~ $_<ipchars>.ast for @$<isegment>;
            make $result;
        }

        method ipath-rootless($/) {
            my $result = '';
            $result ~= $_.ast with $<isegment-nz>;
            $result ~= '/' ~ $_<ipchars>.ast for @$<isegment>;
            make $result;
        }

        method ipath-noscheme($/) {
            make ~$/;
        }

        method ipath-empty($/) {
            make '';
        }

        method isegment($/) {
            make $<ipchars>.ast;
        }

        method isegment-nz($/) {
            make $<ipchars>.ast;
        }

        method iquery($/) {
            my $result = '';
            $result ~= $_<ipchars> ?? $_<ipchars>.ast !! ~$_ for @$0;
            make $result;
        }

        method ifragment($/) {
            my $result = '';
            $result ~= $_<ipchars> ?? $_<ipchars>.ast !! ~$_ for @$0;
            make $result;
        }

        method ipchars($/) {
            my $result = '';
            $result ~= $_<broken> ?? encode-percents(~$_) !! ~$_ for @$0;
            make $result;
        }

        method iref($/) {
            make ($<IRI> // $<irelative-ref>).ast
        }

        method irelative-ref($/) {
            my %parts = $<irelative-part>.ast;
            %parts<query> = .ast with $<iquery>;
            %parts<fragment> = .ast with $<ifragment>;
            make Cro::Iri.bless(|%parts);
        }

        method irelative-part($/) {
            make $<iauthority>
                ?? %( $<iauthority>.ast, path => $<ipath-abempty>.ast )
                !! %( path => ($<ipath-absolute> || $<ipath-noscheme> || $<ipath-empty>).ast );
        }
    }

    #| Parse a IRI into a Cro::Iri object
    method parse(Str() $iri-string, :$grammar = Cro::Iri::GenericParser,
                 :$actions = Cro::Iri::GenericActions.new --> Cro::Iri) {
        with $grammar.parse($iri-string, :$actions) {
            .ast
        } else {
            die X::Cro::Iri::ParseError.new(:$iri-string)
        }
    }

    #| Parse a IRI reference (that is, either an absolute or relative IRI) into
    #| a Cro::Iri object
    method parse-ref(Str() $iri-string, :$grammar = Cro::Iri::GenericParser,
                     :$actions = Cro::Iri::GenericActions.new --> Cro::Iri) {
        with $grammar.parse($iri-string, :$actions, :rule<iref>) {
            .ast
        }
        else {
            die X::Cro::Iri::ParseError.new(:$iri-string)
        }
    }

    #| Parse a relative IRI into a Cro::Iri object (a relative IRI must not
    #| include a scheme)
    method parse-relative(Str() $iri-string, :$grammar = Cro::Iri::GenericParser,
                          :$actions = Cro::Iri::GenericActions.new --> Cro::Iri) {
        with $grammar.parse($iri-string, :$actions, :rule<irelative-ref>) {
            .ast
        }
        else {
            die X::Cro::Iri::ParseError.new(:$iri-string)
        }
    }

    #| Obtain the user part of the user info, if any, with percent sequences
    #| decoded
    method user(--> Str) {
        with $!userinfo {
            decode-percents(.split(":", 2)[0])
        }
        else {
            Str
        }
    }

    #| Obtain the password part of the user info, if any, with percent sequences
    #| decoded (use of this is considered deprecated)
    method password(--> Str) {
        with $!userinfo {
            with .split(":", 2)[1] {
                return decode-percents($_);
            }
        }
        return Str;
    }

    #| Turn the Cro::Iri object into a string representation of the IRI
    multi method Str(Cro::Iri:D: --> Str) {
        my $result = '';
        with $!scheme {
            $result ~= "$_:";
        }
        with $!authority {
            $result ~= "//$_";
        }
        $result ~= $!path;
        with $!query {
            $result ~= "?$_";
        }
        with $!fragment {
            $result ~= "#$_";
        }
        return $result;
    }

    method to-uri(--> Cro::Uri) {
        Cro::Uri.new(
            |(scheme => encode-non-ASCII($_) with $!scheme),
            |(authority => encode-non-ASCII($_) with $!authority),
            |(userinfo => encode-non-ASCII($_) with $!userinfo),
            |(host => encode-non-ASCII($_) with $!host),
            :$!host-class,
            :$!port,
            |(path => encode-non-ASCII($_) with $!path),
            |(query => encode-non-ASCII($_) with $!query),
            |(fragment => encode-non-ASCII($_) with $!fragment),
        );
    }

    sub encode-non-ASCII(Str $s) is export(:encode-percents) {
        $s.subst: :g, /<-[\x00..\x7F]>+/, {
            .Str.encode('utf8').list.map({ $_ > 16 ?? "%" ~ .base(16) !! "%0" ~ .base(16) }).join
        }
    }
}
