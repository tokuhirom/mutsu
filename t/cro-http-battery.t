use Test;

# Cro is bundled (BATTERIES.md §7, docs/batteries/cro-http.md), so
# Cro::Core/Cro::TLS/Cro::HTTP must load and work with a plain `use` -- no
# `-I`, no install. This pins the zero-config resolution and a smoke slice
# of routing/dispatch; the exhaustive behaviour check is the release-time
# gate that runs the full upstream suites (scripts/battery-testsuite.sh).

plan 6;

use Cro;
use Cro::HTTP::Request;
use Cro::HTTP::Router;

my $app = route {
    get -> 'hello', $name {
        content 'text/plain', "Hello, $name!";
    }
    post -> 'echo' {
        request-body -> $body {
            content 'text/plain', $body;
        }
    }
}

ok $app ~~ Cro::Transform, 'route {} gives back a Cro::Transform';

my $source = Supplier.new;
my $responses = $app.transformer($source.Supply).Channel;

$source.emit(Cro::HTTP::Request.new(:method<GET>, :target</hello/world>));
given $responses.receive -> $r {
    is $r.status, 200, 'GET /hello/world matches the route';
    is $r.body-blob.result.decode('utf-8'), 'Hello, world!',
        'the response body is rendered from the path param';
}

$source.emit(Cro::HTTP::Request.new(:method<GET>, :target</nope>));
given $responses.receive -> $r {
    is $r.status, 404, 'an unmatched route gives 404';
}

use JSON::JWT;
my $token = JSON::JWT.encode({ sub => 'alice' }, alg => 'HS256', secret => 'shh');
my %claims = JSON::JWT.decode($token, alg => 'HS256', secret => 'shh');
is %claims<sub>, 'alice', 'JSON::JWT (a Cro::HTTP dependency) round-trips a token';

use Cro::TLS;
ok Cro::TLS::Listener ~~ Cro::Source, 'Cro::TLS (a Cro::HTTP dependency) loads';
