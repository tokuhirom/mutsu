# Because Path is limited in form based on factors outside of it, it doesn't
# actually provide any validation within itself. As such, this type is immutable
# to prevent a problem. This could be made mutable if it is given a reference to
# the parent URI class. The URI class is responsible for performing the
# necessary validations.
unit class URI::Path;

use URI::Escape;

has Str $.path;
has @.segments;

my @rules = <
    path-abempty
    path-absolute
    path-noscheme
    path-rootless
    path-empty
>;

multi method new(URI::Path:U: Match:D $comp, :$scheme) {
    my $path-type = @rules.first({ $comp{ $_ }.defined });
    my $path = $comp{ $path-type };

    my @segments := $path<segment>.list.grep(*.defined).map({.Str}).List || ('', );
    if $path<segment-nz-nc> || $path<segment-nz> -> $first-chunk {
        @segments := ($first-chunk.Str, |@segments);
    }

    if "$path".starts-with('/') {
        @segments := ('', |@segments);
    }

    $path = "$path";

    if $scheme and  $scheme ~~ /http/ {
        $path = $path.split("/").map( { uri-escape( uri-unescape($_) ) } ).join("/"); # Reconstruct path
    }


    self.new(:$path, :@segments);
}

submethod BUILD(:$!path = '', :@segments = $!path.split('/')) {
    @!segments := @segments.List;
}

multi method gist(URI::Path:D: --> Str ) { $!path }
multi method Str(URI::Path:D: --> Str )  { $!path }

method rel2abs(URI::Path:D: Str:D() $base) {
    if $!path.starts-with('/') {
        self;
    }
    else {
        my $path = $base.subst(rx{"/"*$}, '')  ~ '/' ~ $!path;
        self.new: :$path;
    }
}

# vim: expandtab shiftwidth=4
