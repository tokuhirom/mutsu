# Crypt::Random bundled as the CSPRNG battery

`Crypt::Random` (`github:skinkade`, v0.4.1, Artistic-2.0) is vendored at
`modules/Crypt-Random/` and resolves with zero config. All 3 upstream test
files (5 subtests) pass, matching raku, and the whole `Crypt::Random::Extra`
API (UUIDv4, random primes, sampling) works. It is a hard dependency of
Cro::HTTP — this is the first Cro::HTTP dependency locked in behind the
release gate.

Three general interpreter fixes got it there; none is Crypt::Random-specific
and the vendored source is untouched:

- **`nqp::open` / `nqp::readfh` / `nqp::closefh`** — the Nix backend reads
  `/dev/urandom` through these low-level handle ops. `open` maps the MoarVM
  mode letters onto the existing `open_file_handle` machinery (always binary),
  `readfh` REPLACES the buffer's contents with up to N bytes (a short read at
  EOF is not an error), `closefh` closes and returns the handle. Pin:
  `t/nqp-file-ops.t`.
- **`:N[...]` radix lists flatten an embedded iterable.** `:256[@a]` and
  `:256[$buf.values]` treated the array/Seq as ONE digit and numified it to
  its element count, so the UUID builder produced near-zero values. The
  bracket body is an ordinary list constructor; its iterables now flatten
  into the digit list. Pin: `t/radix-list-iterable-flatten.t`.
- **`Match.join` joins the positional captures** (`.list`), not the matched
  string — `($hex ~~ /(........)(....).../).join("-")` came back dash-less. A
  captureless Match joins to the empty string, as in rakudo. Pin:
  `t/match-join-captures.t`.

Packaging: `batteries.lock` row + all 3 files whitelisted in the release gate,
`t/crypt-random-battery.t` smoke test (zero-config `use`, UUID version/variant
bits), the selection record `docs/batteries/crypt-random.md`, the BATTERIES.md
§7 index row, and a site row via `scripts/gen-batteries-manifest.py`.
