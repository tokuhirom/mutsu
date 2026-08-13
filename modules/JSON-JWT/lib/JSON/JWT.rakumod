use JSON::Fast;
use MIME::Base64;
use OpenSSL::Digest;
use OpenSSL::RSATools;
use Digest::HMAC;

unit class JSON::JWT;

multi method decode($jwt, :$alg where 'none') {
    my %pack := self!unpack($jwt);
    %pack<header><alg> ne 'none'
      ?? (die "Header lists signature type != 'none' (%pack<header><alg>)")
      !! %pack<signature>
        ?? (die "Signature exists with signature type = 'none'")
        !! %pack<body>
}
multi method decode($jwt, :$alg where 'HS256', :$secret!) {
    my %pack := self!unpack($jwt);
    %pack<header><alg> ne 'HS256'
      ?? (die "Header lists signature type != 'HS256' (%pack<header><alg>)")
      !! %pack<signature>
        # XXX secure compare? XXX #
        ?? hmac($secret, %pack<sigblob>, &sha256) eq %pack<signature>
          ?? %pack<body>
          !! (die "Signature does not match.")
        !! (die "No signature found.")
}
multi method decode($jwt, :$alg where 'RS256', :$pem!) {
    my %pack := self!unpack($jwt);
    %pack<header><alg> ne 'RS256'
      ?? (die "Header lists signature type != 'RS256' ("~%pack<header><alg>~")")
      !! %pack<signature>
        ?? OpenSSL::RSAKey.new(:public-pem($pem))
             .verify(%pack<sigblob>, %pack<signature>, :sha256)
          ?? %pack<body>
          !! (die "Signature verify failed.")
        !! (die "No signature found.")
}

method decode-noverify($jwt) { self!unpack($jwt)<body> }

method !unpack(Str:D $jwt) {
    my @parts = $jwt.split('.');
    die "JWT does not have 2 or 3 parts; cannot unpack."
      unless @parts == 2 | 3;

    my %pack;
    # MIME::Base64 doesn't do base64url
    my $part1 := @parts[0].trans: '-_' => '+/';
    my $part2 := @parts[1].trans: '-_' => '+/';

    %pack<header> := from-json MIME::Base64.decode-str: $part1;
    die "Not a JWT" if %pack<header><typ> ne 'JWT';

    %pack<body> := from-json MIME::Base64.decode-str: $part2;

    if @parts[2] -> $part3 {
        %pack<signature> :=
          MIME::Base64.decode: $part3.trans: '-_' => '+/';
    }
    %pack<sigblob> := "$part1.$part2".encode: 'ascii';

    %pack
}

multi method encode($data, :$alg where 'none' --> Str:D) {
    self!pack: %(body => $data, header => %(:typ<JWT>, :alg<none>));
}

multi method encode($data, :$alg where 'HS256', :$secret! --> Str:D) {
    my %pack = body => $data, header => %(:typ<JWT>, :alg<HS256>);
    my $sigstring := self!pack: %pack, :signing;
    %pack<signature> = hmac($secret, $sigstring.encode('ascii'), &sha256);

    self!pack(%pack)
}

multi method encode($data, :$alg where 'RS256', :$pem! --> Str:D) {
    my %pack = body => $data, header => %(:typ<JWT>, :alg<RS256>);
    my $sigstring := self!pack: %pack, :signing;
    my $key = OpenSSL::RSAKey.new: :private-pem($pem);
    %pack<signature> = $key.sign: $sigstring.encode('ascii'), :sha256;

    self!pack(%pack)
}

method !pack(%pack, :$signing --> Str:D) {
    my $packed :=
      MIME::Base64.encode-str(to-json(%pack<header>), :oneline)
        .trans('+/' => '-_').subst('=', :g)
      ~ "."
      ~ MIME::Base64.encode-str(to-json(%pack<body>), :oneline)
        .trans('+/' => '-_').subst('=', :g);

    %pack<signature>
      ?? $packed
           ~ "."
           ~ MIME::Base64.encode(%pack<signature>, :oneline)
              .trans('+/' => '-_').subst('=', :g)
      !! $packed
}

=begin pod

=head1 NAME

JSON::JWT - JSON Web Token (JWT) implementation for Raku

=head1 SYNOPSIS

=begin code :lang<raku>

use JSON::JWT;

my $jwt = JSON::JWT.encode($data, :alg<none>);             # no encryption
my $jwt = JSON::JWT.encode($data, :alg<HS256>, :$secret);  # HS256 encryption
my $jwt = JSON::JWT.encode($data, :alg<RS256>, :$pem);     # RS256 encryption

my $data = JSON::JWT.decode($jwt, :alg<none>);             # no encryption
my $data = JSON::JWT.decode($jwt, :alg<HS256>, :$secret);  # HS256 encryption
my $data = JSON::JWT.decode($jwt, :alg<RS256>, :$pem);     # RS256 encryption

=end code

=head1 DESCRIPTION

JSON::JWT provides a class with an implementation of the
JSON Web Token (JWT) standard, with support for C<HS256> and
C<RS256> encryption, or no encryption.

=head1 AUTHOR

Andrew Egeler

Source can be located at: https://github.com/raku-community-modules/JSON-JWT .
Comments and Pull Requests are welcome.

=head1 COPYRIGHT AND LICENSE

Copyright 2017 - 2018 Andrew Egeler

Copyright 2019 - 2024 Raku Community

All files in this repository are licensed under the terms of Create Commons
License; for details please see the LICENSE file

=end pod

# vim: expandtab shiftwidth=4
