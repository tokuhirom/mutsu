use Tubu::Request;
use Tubu::Response;

unit module Tubu;

# ---- Route table (module singleton) ----
my class Route {
    has Str $.method;
    has Str $.pattern;        # regex source string for the path
    has @.param-names;
    has &.handler;
}

my @routes;

# Compile a Sinatra-style path ('/post/:id') into a regex SOURCE string plus
# capture names. (Kept as a string, not a Regex object, because the matcher is
# built fresh at dispatch time with the pattern lexically in scope.)
sub compile-path(Str $path) {
    my @names;
    my $pat = $path.subst(/':' (\w+)/, -> $m { @names.push(~$m[0]); '(<-[/]>+)' }, :g);
    return $pat, @names;
}

sub add-route(Str $method, Str $path, &handler) {
    my ($pat, @names) = compile-path($path);
    @routes.push(Route.new(method => $method, pattern => $pat, param-names => @names, handler => &handler));
}

# ---- Exported route declarators (Sinatra/Bailador style: `get '/' => sub {...}`) ----
sub get(Pair $r)    is export { add-route('GET', $r.key, $r.value) }
sub post(Pair $r)   is export { add-route('POST', $r.key, $r.value) }
sub put(Pair $r)    is export { add-route('PUT', $r.key, $r.value) }
sub del(Pair $r)    is export { add-route('DELETE', $r.key, $r.value) }

# ---- Current request accessor ----
sub request() is export {
    return $*TUBU-REQUEST;
}

# ---- before-hooks (middleware): run before each dispatched request ----
my @before-hooks;
sub before(&hook) is export { @before-hooks.push(&hook) }

# ---- Response helpers (Sinatra-style) ----
sub json($data) is export {
    my $body = $data ~~ Str ?? $data !! $data.Str;
    return Tubu::Response.new(
        status => 200,
        headers => ['Content-Type' => 'application/json'],
        body => $body,
    );
}

sub redirect(Str $location, Int $code = 302) is export {
    return Tubu::Response.new.redirect($location, $code);
}

sub html(Str $body) is export {
    my $r = Tubu::Response.new(body => $body);
    $r.header('Content-Type', 'text/html; charset=utf-8');
    return $r;
}

# ---- Normalize a handler return value into a Tubu::Response ----
sub to-response($result) {
    return $result if $result ~~ Tubu::Response;
    my $r = Tubu::Response.new;
    if $result ~~ Positional && $result.elems == 3 {
        # already a P6W-ish triple
        $r.status = $result[0];
        for @($result[1]) -> $h { $r.headers.push($h) }
        $r.body = $result[2] ~~ Positional ?? $result[2].join !! $result[2].Str;
    } else {
        $r.body = $result.Str;
        $r.header('Content-Type', 'text/html; charset=utf-8');
    }
    return $r;
}

# ---- Dispatch one request (P6W env -> Response) ----
sub dispatch(%env) is export {
    my $req = Tubu::Request.new(%env);
    my $method = $req.method;
    my $path = $req.path;

    for @routes -> $route {
        next unless $route.method eq $method;
        my $pat = $route.pattern;
        if $path ~~ / ^ <$pat> $ / {
            my %caps;
            my @names = $route.param-names;
            for ^@names.elems -> $i {
                %caps{@names[$i]} = ~$/[$i];
            }
            $req.captures = %caps;
            my $*TUBU-REQUEST = $req;
            for @before-hooks -> &hook { hook($req) }
            my &h = $route.handler;
            my $result;
            if &h.signature.params.elems > 0 {
                $result = &h(|@names.map({ %caps{$_} }));
            } else {
                $result = &h();
            }
            return to-response($result);
        }
    }
    # no route matched
    my $r = Tubu::Response.new(status => 404);
    $r.header('Content-Type', 'text/plain');
    $r.body = 'Not Found';
    return $r;
}

# ---- Produce a P6W app closure ----
sub to-app() is export {
    return sub (%env) {
        my $resp = dispatch(%env);
        return $resp.finalize;
    };
}
