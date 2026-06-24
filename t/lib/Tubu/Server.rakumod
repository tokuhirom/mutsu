unit class Tubu::Server;

# A minimal *synchronous* HTTP/1.0 server that drives a P6W app
# (sub (%env) { return $status, @headers, @body }). One request per connection.

has Str $.host = '127.0.0.1';
has Int $.port = 8080;

# Parse a raw HTTP request (headers + optional body) into a P6W-style %env.
sub parse-request(Str $raw) {
    my %env;
    my ($head, $body) = $raw.split("\r\n\r\n", 2);
    $body //= '';
    my @lines = $head.split("\r\n");
    my $request-line = @lines.shift // '';
    my ($method, $target, $proto) = $request-line.split(' ');
    $method //= 'GET';
    $target //= '/';
    my ($path, $query) = $target.split('?', 2);
    %env<REQUEST_METHOD>  = $method;
    %env<PATH_INFO>       = $path;
    %env<QUERY_STRING>    = $query // '';
    %env<SERVER_PROTOCOL> = $proto // 'HTTP/1.0';
    for @lines -> $line {
        next unless $line;
        my ($k, $v) = $line.split(':', 2);
        next unless $v.defined;
        my $key = 'HTTP_' ~ $k.trim.uc.subst('-', '_', :g);
        %env{$key} = $v.trim;
    }
    %env<CONTENT_LENGTH> = %env<HTTP_CONTENT_LENGTH> if %env<HTTP_CONTENT_LENGTH>:exists;
    %env<p6w.input.body> = $body;
    return %env;
}

my %CONTENT-TYPES =
    'html' => 'text/html; charset=utf-8',
    'css'  => 'text/css',
    'js'   => 'application/javascript',
    'json' => 'application/json',
    'txt'  => 'text/plain; charset=utf-8',
    'png'  => 'image/png',
    'svg'  => 'image/svg+xml';

#| Serve a file from disk: returns a P6W triple. 404 if missing.
our sub static-file(Str $path) is export {
    return (404, ['Content-Type' => 'text/plain'], ('Not Found',)) unless $path.IO.e;
    my $ext = $path.IO.extension;
    my $ct = %CONTENT-TYPES{$ext} // 'application/octet-stream';
    my $content = $path.IO.slurp;
    return (200, ['Content-Type' => $ct], ($content,));
}

my %STATUS-TEXT =
    200 => 'OK', 201 => 'Created', 204 => 'No Content',
    301 => 'Moved Permanently', 302 => 'Found', 304 => 'Not Modified',
    400 => 'Bad Request', 401 => 'Unauthorized', 403 => 'Forbidden',
    404 => 'Not Found', 405 => 'Method Not Allowed', 500 => 'Internal Server Error';

# Build a raw HTTP response string from a P6W triple.
sub build-response($status, $headers, $body) {
    my $text = %STATUS-TEXT{$status} // 'OK';
    my $body-str = $body ~~ Positional ?? $body.join !! $body.Str;
    my %seen;
    my $out = "HTTP/1.0 $status $text\r\n";
    for @($headers) -> $h {
        $out ~= "{$h.key}: {$h.value}\r\n";
        %seen{$h.key.lc} = True;
    }
    $out ~= "Content-Length: {$body-str.encode.bytes}\r\n" unless %seen<content-length>;
    $out ~= "Connection: close\r\n";
    $out ~= "\r\n";
    $out ~= $body-str;
    return $out;
}

# Public: turn a raw request into a raw response by running the app. This is the
# pure, network-free core (used for testing without binding a socket).
method handle-raw(&app, Str $raw) {
    my %env = parse-request($raw);
    my ($status, $headers, $body) = app(%env);
    return build-response($status, $headers, $body);
}

# Public: bind a socket and serve requests synchronously until interrupted.
method run(&app) {
    my $listen = IO::Socket::INET.new(
        :localhost($.host), :localport($.port), :listen,
    );
    say "Tubu listening on http://$.host:$.port/";
    loop {
        my $conn = $listen.accept;
        my $raw = $conn.recv;
        my $resp = self.handle-raw(&app, $raw);
        $conn.print($resp);
        $conn.close;
    }
}
