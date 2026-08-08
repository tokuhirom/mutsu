# Cro's session tests fail on a private method of a monitor

`t/http-session-inmemory.rakutest` now passes its first two tests and stops at:

```
No such private method 'get-cookie-lifetime' for invocant of type 'Cro::HTTP::Client::CookieJar'
  in block <unit> at .../Cro/HTTP/Client/CookieJar.rakumod line 42
```

`Cro::HTTP::Client::CookieJar` is declared with OO::Monitors' `monitor`
declarator, and `get-cookie-lifetime` is one of its private methods
(`method !get-cookie-lifetime(...)`). Private-method dispatch on a monitor
invocant does not find it.

Likely related to how `MetamodelX::MonitorHOW.add_method` installs the lock
wrapper: it skips `BUILDALL`/`POPULATE`/`clone` but wraps everything else, and
mutsu's private-method lookup (`runtime/methods_qualified.rs`,
`runtime/methods_instance_ops.rs`) may consult a table the wrap/HOW path does not
populate for `!`-prefixed names.

## History

The earlier symptom on this file was an empty response body for every request.
That had two causes, both fixed:

* `news/2026-08/prefix-incdec-on-an-rw-accessor.md` — the route body writes
  `'Visit ' ~ ++$session.count`, and prefix `++` on an rw accessor was
  unimplemented;
* `news/2026-08/topic-alias-does-not-cross-frames.md` — `given
  Cro::HTTP::Client.new -> $client { … }` lost `$client` mid-request.

## How to reproduce

```
bash tmp/cro-t.sh t/http-session-inmemory.rakutest
```

(helper scripts and the vendored Cro checkout live under `tmp/`, which is
gitignored.)
