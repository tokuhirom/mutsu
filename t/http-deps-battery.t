use Test;

# The HTTP client's dependency layer is bundled (BATTERIES.md §7,
# docs/batteries/http-deps.md), so each of these must load and work with a plain
# `use` -- no `-I`, no install. This pins the zero-config resolution and a smoke
# slice of each module's API; the exhaustive behaviour check is the release-time
# gate that runs their full upstream suites (scripts/battery-testsuite.sh).

plan 15;

use URI;
my $u = URI.new('https://example.com:8443/a/b?x=1&y=2#frag');
is $u.scheme, 'https', 'URI parses the scheme';
is $u.host, 'example.com', 'URI parses the host';
is $u.port, 8443, 'URI parses the port';
is $u.path, '/a/b', 'URI parses the path';
is $u.fragment, 'frag', 'URI parses the fragment';
is $u.query<x>[0], '1', 'URI exposes query parameters';

use MIME::Base64;
is MIME::Base64.encode-str('hello'), 'aGVsbG8=', 'MIME::Base64 encodes';
is MIME::Base64.decode-str('aGVsbG8='), 'hello', 'MIME::Base64 decodes';

use HTTP::Status;
is get_http_status_msg(404), 'Not Found', 'HTTP::Status names a status code';
ok is-client-error(404), 'HTTP::Status classifies a client error';

use DateTime::Parse;
is DateTime::Parse.new('Sun, 06 Nov 1994 08:49:37 GMT').Date, '1994-11-06',
    'DateTime::Parse reads an RFC 1123 date';

use Encode;
is Encode::decode('iso-8859-2', buf8.new(0xa3)), 'Ł', 'Encode decodes latin-2';
is Encode::decode('utf-8', buf8.new(0xc5, 0x81)), 'Ł', 'Encode decodes utf-8';

use File::Directory::Tree;
my $root = 'tmp/http-deps-battery-t';
mktree "$root/a/b";
ok "$root/a/b".IO.d, 'File::Directory::Tree creates a tree';
rmtree $root;
nok $root.IO.e, 'File::Directory::Tree removes a tree';
