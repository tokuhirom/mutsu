# A Cro::HTTP::Client response arrives with headers but an empty body

With `Promise($supply)` completing correctly
(`news/2026-08/promise-of-a-supply-completes.md`), a mutsu `Cro::HTTP::Client`
now completes a real HTTP round trip against a mutsu `Cro::HTTP::Server` in the
same process — but only the status line and headers survive. The body is empty.

Repro (needs the vendored Cro dists on `-I`; `tmp/cc2.p6` in a working tree):

```raku
use Cro::HTTP::Router;
use Cro::HTTP::Server;
use Cro::HTTP::Client;

my $app = route { get -> 'hello' { content 'text/plain', 'world' } }
my $service = Cro::HTTP::Server.new(:host('localhost'), :port(31998), application => $app);
$service.start;

my $resp = await Cro::HTTP::Client.get("http://localhost:31998/hello");
say $resp.status;                       # 200            -- right
say $resp.header('content-length');     # 5              -- right
say (await $resp.body-blob).elems;      # 0              -- WRONG, should be 5
say await $resp.body-text;              # ''             -- WRONG, 'world'
```

The `content-length: 5` header proves the response the server serialized was
complete and that the client parsed its headers, so the five body bytes either
never reach `Cro::HTTP::ResponseParser` or are parsed and then dropped before
the `Cro::HTTP::Response`'s body Supply is tapped.

Things worth checking first:

- Cro emits the `Cro::HTTP::Response` as soon as its headers are parsed and
  streams the body separately, so the body bytes travel through the *same*
  connection Supply *after* the response object has already been handed to the
  client. Does the client-side pipeline keep consuming the connection after
  `$!next-response-vow.keep($_)`?
- `Cro::HTTP::Client::Pipeline`'s `send-request` closes over `$!in`/`$!tap`; the
  request path calls `$pipeline.close` right after the response (the type-object
  `self` makes `self.persistent` falsy, so the `else` branch runs). Check the
  close is not tearing the connection down before the body is read.
- The request carries `Connection: close`, so the server closes the socket after
  the body — a lost `Done` or a dropped final read on the client's
  `.Supply(:bin)` would look exactly like this.

Affected: the whole client half of the vendored Cro::HTTP suite —
`t/http-auth-basic.rakutest`, `t/http-auth-basic-with-session.rakutest`,
`t/http-session-inmemory.rakutest`, `t/http-session-persistent.rakutest`,
`t/http-middleware.rakutest`, `t/router-auth.rakutest`, and the round-trip half
of `t/http-router.rakutest`. All of them now reach their assertions and fail on
an empty body rather than dying on `Any`.
