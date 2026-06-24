unit class Tubu::Response;

has Int $.status is rw = 200;
has @.headers;        # array of Pair (name => value)
has $.body is rw = '';

method new(:$status = 200, :@headers, :$body = '') {
    self.bless(:$status, :@headers, :$body);
}

method header(Str $name, Str $value) {
    @.headers.push($name => $value);
    return self;
}

method content-type(Str $ct) {
    self.header('Content-Type', $ct);
}

method cookie(Str $name, Str $value, *%opts) {
    my $c = "$name=$value";
    $c ~= "; Path=" ~ %opts<path> if %opts<path>:exists;
    $c ~= "; Max-Age=" ~ %opts<max-age> if %opts<max-age>:exists;
    $c ~= "; HttpOnly" if %opts<httponly>;
    @.headers.push('Set-Cookie' => $c);
    return self;
}

method redirect(Str $location, Int $code = 302) {
    $.status = $code;
    @.headers.push('Location' => $location);
    $.body = '';
    return self;
}

# Produce the P6W triple: (status, headers, body-list)
method finalize() {
    my $b = $.body;
    my @body-list = $b ~~ Blob ?? ($b,) !! ($b.Str,);
    return $.status, @.headers, @body-list;
}
