use Test;
use lib 't/lib';
use Tubu;
use Tubu::Server;

# Tubu is a minimal *synchronous*, P6W-compatible, Sinatra-style web framework
# written in pure Raku (t/lib/Tubu*). This exercises its core end-to-end WITHOUT
# binding a socket (the network-free `handle-raw` path) so it is deterministic
# in CI. It demonstrates that mutsu can run a real web framework: routing, path
# params, query/form params, cookies, before-hooks, JSON/HTML/redirect helpers,
# 404, and a from-scratch HTTP request parser + response builder.
#
# Depends on:
#  - imported route declarators (`get`/`post`) shadowing same-named core
#    builtins (`get` is the IO line-reader) — see t/imported-sub-shadows-builtin.t
#  - a caller's readonly param not poisoning a callee's same-named `my` var
#    (the request parser does `my ($head,$body)=...`) — see t/readonly-param-shadow.t

plan 18;

get '/' => sub { 'home' };
get '/hello/:name' => sub ($name) { "hi $name" };
get '/search' => sub { 'q=' ~ (request.param('q') // '') };
post '/submit' => sub { 'posted ' ~ (request.param('msg') // '') };
get '/j' => sub { json('{"ok":1}') };
get '/r' => sub { redirect('/') };
get '/cookie' => sub {
    my $n = (request.cookies<n> // '0').Int + 1;
    my $resp = html("n=$n");
    $resp.cookie('n', $n.Str, :path</>);
    $resp;
};

my @hooks;
before sub ($req) { @hooks.push($req.path) };

my &app = to-app();
my $srv = Tubu::Server.new;

sub req($method, $path, :$query = '', :$body = '', :$cookie = '') {
    my $c = $cookie ?? "Cookie: $cookie\r\n" !! '';
    my $target = $query ?? "$path?$query" !! $path;
    my $raw = "$method $target HTTP/1.0\r\nHost: x\r\n{$c}\r\n$body";
    return $srv.handle-raw(&app, $raw);
}

my $home = req('GET', '/');
ok $home.contains('200 OK'), 'GET / -> 200';
ok $home.contains('home'), 'GET / body';

ok req('GET', '/hello/world').contains('hi world'), 'path param captured';
ok req('GET', '/search', query => 'q=raku').contains('q=raku'), 'query param parsed';
ok req('POST', '/submit', body => 'msg=hi+there').contains('posted hi there'),
    'form body param parsed + url-decoded';

my $j = req('GET', '/j');
ok $j.contains('application/json'), 'json helper sets content-type';
ok $j.contains('"ok":1'), 'json helper body';

my $r = req('GET', '/r');
ok $r.contains('302 Found'), 'redirect status';
ok $r.contains('Location: /'), 'redirect Location header';

ok req('GET', '/nope').contains('404 Not Found'), 'unmatched route -> 404';

my $c1 = req('GET', '/cookie');
ok $c1.contains('Set-Cookie: n=1'), 'cookie set to 1 on first visit';
ok $c1.contains('n=1'), 'cookie body shows n=1';
my $c2 = req('GET', '/cookie', cookie => 'n=4');
ok $c2.contains('Set-Cookie: n=5'), 'cookie incremented to 5';
ok $c2.contains('n=5'), 'cookie body shows n=5';

ok $home.contains('Content-Length: 4'), 'Content-Length computed for "home"';

ok @hooks.elems >= 5, 'before-hook fired for each dispatched request';
is @hooks[0], '/', 'first hook saw /';
is @hooks[1], '/hello/world', 'second hook saw /hello/world';
