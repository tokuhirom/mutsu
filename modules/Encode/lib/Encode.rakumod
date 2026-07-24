unit module Encode;

use Encode::Latin2;
use Encode::Windows1252;
use Encode::Windows1251;

my class X::Encode::Unknown is Exception {
    has $.encoding;

    method message {
        "Unknown encoding $.encoding."
    }
}

sub latin2(Buf $buf) {
    $buf.list.map({ %Encode::Latin2::map{$_} // $_ })>>.chr.join;
}

sub latin1(Buf $buf) {
    $buf.decode('iso-8859-1');
}

sub utf8(Buf $buf) {
    $buf.decode('utf8');
}

sub ascii(Buf $buf) {
    $buf.decode('ascii');
}

sub cp1251(Buf $buf) {
    $buf.list.map({ %Encode::Windows1251::map{$_} // $_ })>>.chr.join;
}

sub cp1252(Buf $buf) {
    $buf.list.map({ %Encode::Windows1252::map{$_} // $_ })>>.chr.join;
}

my %encodings =
    'iso-8859-2' => &latin2,
    'iso_8859-2' => &latin2,
    'latin2'     => &latin2,
    'latin-2'    => &latin2,

    'iso-8859-1' => &latin1,
    'iso_8859-1' => &latin1,
    'latin1'     => &latin1,
    'latin-1'    => &latin1,

    'windows-1251' => &cp1251,
    'windows1251'  => &cp1251,
    'cp-1251'      => &cp1251,
    'cp1251'       => &cp1251,

    'windows-1252' => &cp1252,
    'windows1252'  => &cp1252,
    'cp-1252'      => &cp1252,
    'cp1252'       => &cp1252,

    'utf8'       => &utf8,
    'utf-8'      => &utf8,

    'ascii'      => &ascii,
;

our sub decode($encoding, Buf $buf) {
    X::Encode::Unknown.new(:encoding($encoding)).throw unless %encodings{$encoding}.defined;

    &(%encodings{$encoding})($buf);
}

=begin pod

=head1 NAME

Encode - character encodings

=head1 SYNOPSIS

    use Encode;
    say Encode::decode('latin2', Buf.new(0xa3));

=head1 DESCRIPTION

=head2 Available encodings:

=item utf-8 (utf8)
=item iso-8859-2 (latin2)
=item iso-8859-1 (latin1)
=item windows-1251 (cp-1251)
=item windows-1252 (cp-1252)
=item ascii

=head1 ROUTINES

=head2 routine decode

    sub decode($encoding, Buf $buf) returns Str

Returns decoded $buf.

Throws X::Encode::Unknown exception if $encoding is not implemented.

=head1 AUTHOR

Filip Sergot (sergot)
Website: filip.sergot.pl
Contact: filip (at) sergot.pl

=end pod
