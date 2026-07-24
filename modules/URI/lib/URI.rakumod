use URI::Path;
use URI::Query;

unit class URI;

use IETF::RFC_Grammar;
use IETF::RFC_Grammar::URI;
use URI::Escape;
need URI::DefaultPort;

constant &split-query is export(:split-query) = &URI::Query::split-query;

class X::URI::Invalid is Exception {
    has $.source;
    method message { "Could not parse URI: $!source" }
}

class X::URI::Path::Invalid is X::URI::Invalid {
    has $.path;
    has $.bad-segment;
    method message {
        qq[Could not parse path "$!path" as part of URI: $.source]
        ~ ($!bad-segment ?? qq[ with bad segment "$!bad-segment"] !! '')
    }
}

our subset Scheme of Str
    where /^ [
           ''
        || <IETF::RFC_Grammar::URI::scheme>
    ] $/;
our subset Userinfo of Str
    where /^ [ <IETF::RFC_Grammar::URI::userinfo> ] $/;
our subset Host of Str
    where /^ [ <IETF::RFC_Grammar::URI::host> ] $/;
our subset Port of UInt;

class Authority {
    has Userinfo:D $.userinfo is default('') is rw = '';
    has Host:D $.host is default('') is rw = '';
    has Port $.port is rw;

    multi method new(Authority:U: Match:D $auth) {
        my Str $userinfo = do with $auth<userinfo> { .Str } else { '' }
        my Str $host     = "$auth<host>".lc;
        my UInt $port    = do with $auth<port> { .Int } else { UInt }

        self.new(:$userinfo, :$host, :$port);
    }

    multi method gist(Authority:D: --> Str ) {
        my $authority = "$!userinfo@" if $!userinfo;
        $authority ~= $!host;
        $authority ~= ":$!port" if $!port;
        $authority;
    }

    multi method Str(Authority:D: --> Str ) { $.gist }
}



our subset Fragment of Str
    where /^ <IETF::RFC_Grammar::URI::fragment> $/;

has Str $.query-form-delimiter;
has $.grammar;
has Bool $.match-prefix = False;
has Path $.path = Path.new;
has Scheme $.scheme = '';
has Authority $.authority is rw;
has Query $.query = Query.new('');
has Fragment $.fragment is rw = '';
has %!query-form; # cache query-form

method parse(URI:D: Str() $str, Bool :$match-prefix = $!match-prefix) {

    # clear string before parsing
    my Str $c_str = $str;
    $c_str .= subst(/^ \s* ['<' | '"'] /, '');
    $c_str .= subst(/ ['>' | '"'] \s* $/, '');

    $!scheme    = '';
    $!authority = Nil;
    $!path      = Path.new;
    $!query.query('');
    $!fragment  = '';

    $!match-prefix = $match-prefix;
    if $!match-prefix {
        $!grammar.subparse($c_str);
    }
    else {
        $!grammar.parse($c_str);
    }

    # hacky but for the time being an improvement
    if (not $!grammar.parse-result) {
        X::URI::Invalid.new(source => $str).throw
    }

    my $comp_container = $!grammar.parse-result<URI-reference><URI> ||
        $!grammar.parse-result<URI-reference><relative-ref>;

    $!scheme = .lc with $comp_container<scheme>;
    $!query.query(.Str) with $comp_container<query>;
    $!fragment = .lc with $comp_container<fragment>;
    $comp_container = $comp_container<hier-part> || $comp_container<relative-part>;

    with $comp_container<authority> -> $auth {
        $!authority = Authority.new($auth);
    }

    $!path = Path.new($comp_container, :$!scheme);
}

# new can pass alternate grammars some day ...
submethod BUILD(:$match-prefix) {
    $!match-prefix = ? $match-prefix;
    $!grammar = IETF::RFC_Grammar.new('rfc3986');
}

multi method new(URI:U: $uri, Bool :$match-prefix) {
    my $obj = self.bless(:$match-prefix);

    $obj.parse($uri) if $uri.defined;
    $obj;
}

multi method new(URI:U: :$uri, Bool :$match-prefix) {
    self.new($uri, :$match-prefix);
}

multi method COERCE(Str:D $uri) { self.new($uri) }

method has-scheme(URI:D: --> Bool:D ) {
    $!scheme.chars > 0;
}
method has-authority(URI:D: --> Bool:D ) {
    $!authority.defined;
}

my regex path-authority {
    [ $<path-abempty> = <IETF::RFC_Grammar::URI::path-abempty> ]
}
my regex path-scheme {
    [   $<path-absolute> = <IETF::RFC_Grammar::URI::path-absolute>
    |   $<path-rootless> = <IETF::RFC_Grammar::URI::path-rootless>
    |   $<path-empty>    = <IETF::RFC_Grammar::URI::path-empty>
    ]
}
my regex path-plain {
    [   $<path-absolute> = <IETF::RFC_Grammar::URI::path-absolute>
    |   $<path-noscheme> = <IETF::RFC_Grammar::URI::path-noscheme>
    |   $<path-empty>    = <IETF::RFC_Grammar::URI::path-empty>
    ]
}

method !check-path(
    :$has-authority = $.has-authority,
    :$has-scheme    = $.has-scheme,
    :$path          = $.path,
    :$source        = $.gist,
) {
    given X::URI::Path::Invalid.new(:$path, :$source) -> \ex {
        if $has-authority {
            ex.throw unless $path ~~ /^ <path-authority> $/;
            return $<path-authority>;
        }
        elsif $has-scheme {
            ex.throw unless $path ~~ /^ <path-scheme> $/;
            return $<path-scheme>;
        }
        else {
            ex.throw unless $path ~~ /^ <path-plain> $/;
            return $<path-plain>;
        }
    }
}

multi method scheme(URI:D: --> Scheme:D ) { $!scheme }
multi method scheme(URI:D: Str() $scheme --> Scheme:D ) {
    self!check-path(:has-scheme, :source(self!gister(:$scheme)));
    $!scheme = $scheme;
}

multi method userinfo(URI:D: --> Userinfo ) {
    return .userinfo with $!authority;
    '';
}

multi method userinfo(URI:D: Str() $userinfo --> Userinfo ) {
    with $!authority {
        .userinfo = $userinfo;
    }
    elsif $userinfo {
        self!check-path(:has-authority, :source(self!gister(:authority("$userinfo@"))));
        $!authority .= new(:$userinfo);
        $!authority.userinfo;
    }
}

multi method host(URI:D: --> Host ) {
    return .host with $!authority;
    '';
}

multi method host(URI:D: Str() $host --> Host ) {
    with $!authority {
        .host = $host;
    }
    elsif $host {
        self!check-path(:has-authority, :source(self!gister(:authority($host))));
        $!authority .= new(:$host);
        $!authority.host;
    }
}

method default-port(URI:D: --> Port ) {
    URI::DefaultPort.scheme-port($.scheme)
}
method default_port(URI:D: --> Port ) { $.default-port } # artifact form

multi method _port(URI:D: --> Port ) {
    return .port with $!authority;
    Nil;
}

multi method _port(URI:D: Nil --> Port ) {
    .port = Nil with $!authority;
    Nil;
}

multi method _port(URI:D: Int() $port --> Port ) {
    with $!authority {
        .port = $port;
    }
    else {
        self!check-path(:has-authority, :source(self!gister(:authority(":$port"))));
        $!authority .= new(:$port);
        $!authority.port;
    }
}


multi method port(URI:D: --> Port ) { $._port // $.default-port }

multi method port(URI:D: |c --> Port ) { self._port(|c) // $.default-port }

proto method authority(|c) { * }

multi method authority(URI:D: Str() $authority --> Authority:D ) {
    my $gist;
    without $!authority {
        self!check-path(:has-authority,
            source => $gist //= self!gister(:$authority),
        );
    }

    $!grammar.parse($authority, rule => 'authority');
    if not $!grammar.parse-result {
        X::URI::Invalid.new(
            source => $gist // self!gister(:$authority),
        ).throw;
    }

    $!authority = Authority.new($!grammar.parse-result);
}

multi method authority(URI:D: Nil --> Authority:U ) {
    with $!authority {
        self!check-path(:!has-authority,
            source => self!gister(authority => ''),
        );
    }

    $!authority = Nil;
}

multi method authority(URI:D: --> Authority ) is rw {
    $!authority;
}

multi method path(URI:D: --> Path:D ) { $!path }

method !make-path(Str $path --> Path:D ) {
    if $path {
        my $comp = self!check-path(
            :$path,
            source => self!gister(:$path),
        );

        Path.new($comp);
    }
    else {
        Path.new;
    }
}

multi method path(URI:D: Str() $path --> Path:D ) {
    $!path = self!make-path($path);
}

multi method segments(URI:D: --> List:D ) { $!path.segments }

multi method segments(URI:D: @segments where *.elems > 0 --> List:D ) {
    my $path = @segments.join('/');
    given self!gister(:$path) -> $source {
        for @segments -> $bad-segment {
            X::URI::Path::Invalid.new(
                :$source,
                :$path,
                :$bad-segment,
            ).throw if $bad-segment.contains("/");
        }

        self!check-path(:$path, :$source);
    }

    $!path = Path.new(
        path     => $path,
        segments => @segments.map(*.Str),
    );
    $!path.segments;
}

multi method segments(URI:D: $first-segment, *@remaining-segments --> List:D ) {
    my @segments = $first-segment, |@remaining-segments;
    self.segments(@segments);
}

# The absolute and relative methods are artifacts carried over from an old
# version of the module.  The perl odule does not provide such
# functionality.  The Ruby equivalent just checks for the presence or
# absence of a scheme.  The URI rfc does identify absolute URIs and
# absolute URI paths and these methods somewhat confused the two.  Their
# functionality at the URI level is no longer seen as needed and is
# being removed.

method absolute is DEPRECATED {
    return Bool.new;
}

method relative is DEPRECATED {
    return Bool.new;
}

method is-absolute { so ($!scheme || $.host) }
method is-relative { ! $.is-absolute }

method directory(URI:D:) {
    my Str $dir = $!path.Str;
    $dir .= subst(/<- [/]>*$/, '');
    $dir ||= self.is-absolute ?? '/' !! './';
    my Path $path = self!make-path($dir);
    self.clone: :$path;
}

method rel2abs(URI:D: URI:D $base --> URI:D) {
    if $.is-absolute {
        self;
    }
    else {
        my URI::Path $path = $!path.rel2abs($base);
        $base.clone: :$path;
    }
}

multi method query(URI:D: --> URI::Query:D ) { $!query }

multi method query(URI:D: Str() $new --> URI::Query:D ) {
    $!query.query($new);
    $!query
}

multi method query(URI:D: *@new, *%bad --> URI::Query:D ) {
    warn 'The query was passed values as named arguments which are ignored. Did you mean to make sure your pairs were quoted or passed in a list instead?' if %bad;

    $!query.query-form(@new);
    $!query
}


proto method query-form(|c) { * }

multi method query-form(URI:D: |c) is DEPRECATED("method query") {
    $!query.query-form(|c)
}

method query_form() is DEPRECATED("method query")  {
    $.query
}

method path-query(URI:D: --> Str:D ) {
    $.query ?? "$.path?$.query" !! "$.path"
}

method path_query { $.path-query } #artifact form

multi method fragment(URI:D: --> Fragment ) { return-rw $!fragment }
multi method fragment(URI:D: Str() $new --> Fragment ) { $!fragment = $new }

method frag(URI:D:) { $.fragment }

method !gister(
    :$scheme = $.scheme,
    :$authority = $.authority,
    :$path = $.path,
    :$query = $.query,
    :$fragment = $.fragment,
) {
    my Str $s;
    $s ~= "$scheme:"     if $scheme;
    $s ~= "//$authority" if $authority;
    $s ~= $path;
    $s ~= "?$query"      if $query;
    $s ~= "#$fragment"   if $fragment;
    $s;
}

multi method gist(URI:D: --> Str:D ) { self!gister }
multi method Str(URI:D: --> Str:D ) { $.gist }

# chunks now strongly deprecated
# it's segments in p5 URI and segment is part of rfc so no more chunks soon!
method chunks() is DEPRECATED("method segments") {
    $!path.segments;
}

multi method query-form() { $!query }

multi method query-form(|c) { $!query.query-form(|c) }

# vim: expandtab shiftwidth=4
