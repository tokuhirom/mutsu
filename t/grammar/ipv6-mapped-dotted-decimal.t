use Test;

# IPv4-mapped IPv6 addresses use a dotted-decimal suffix.  The grammar from
# IP::Addr temporarily lowers the hextet limit while trying this form; the
# dynamic declaration must survive the ratcheted subrule path and its
# backtracking alternatives (GH #8753).

role IPv4-Basic {
    token ipv4 { <octet> ** 4 % '.' }
    token octet { \d ** 1..3 <?{ $/.Int < 256 }> }
}

grammar IPv6-Grammar does IPv4-Basic {
    rule TOP {
        :my $*MAX-HEXTETS = 8;
        <v6-variants>
    }

    rule v6-variants {
        <cidr> | <range> | <scoped> | <ipv6>
    }

    token ipv6 {
        <full-v6> | <mapped-v6> | <compressed-v6>
    }

    token cidr { <ipv6> '/' <prefix-len> }
    rule range { <ipv6> '-' <ipv6> }
    token scoped { <ipv6> '%' $<scope>=( \S+ ) }

    token full-v6 { <hextet> ** { $*MAX-HEXTETS } % ':' }

    sub hextets2int ( @hx ) {
        my $pfx = 0;
        $pfx = ( $pfx +< 16 ) +| $_ for @hx;
        $pfx
    }

    token mapped-v6 {
        :temp $*MAX-HEXTETS = 6;
        [
            <full-v6> <?{
                hextets2int( $/<full-v6><hextet>.map: { (~$_).parse-base( 16 ) } ) ==
                    0xffff | 0xffff0000 | 0x64ff9b0000000000000000
            }> ':'
            | [
                <compressed-v6> <?{ (~$/).ends-with( '::' ) }>
                | <compressed-v6> ':'
            ] <?{
                my @pfx = $/<compressed-v6><sub-v6>[0]<hextet>.map: { (~$_).parse-base(16) };
                my @sfx = $/<compressed-v6><sub-v6>[1]<hextet>.map: { (~$_).parse-base(16) };
                my @zero = 0 xx ($*MAX-HEXTETS - @pfx.elems - @sfx.elems);
                hextets2int( (@pfx, @zero, @sfx).flat ) ==
                    0xffff | 0xffff0000 | 0x64ff9b0000000000000000
            }>
        ]
        <ipv4>
    }

    token compressed-v6 {
        <sub-v6> $<double-col>='::' <sub-v6>
        <?{ ($/<sub-v6>[0]<hextet>.elems + $/<sub-v6>[1]<hextet>.elems) < $*MAX-HEXTETS }>
    }

    token sub-v6 {
        <hextet> ** { ^($*MAX-HEXTETS - 1) } % ':'
    }

    token hextet { <xdigit> ** 1..4 <!before '.' > }
    token prefix-len { <digit> ** ^4 <?{ $/.Int <= 128 }> }
}

my @addresses =
    '0000:0000:0000:0000:0000:ffff:192.168.13.1',
    '::ffff:192.168.13.1',
    '::ffff:0:192.168.13.1',
    '64:ff9b::192.168.13.1';

plan @addresses.elems;
for @addresses -> $address {
    ok IPv6-Grammar.parse($address), "parses $address";
}
