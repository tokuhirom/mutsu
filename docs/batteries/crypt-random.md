# Battery: secure randomness (CSPRNG) — `Crypt::Random`

**Slot:** Secure randomness (CSPRNG) · **Chosen:** `Crypt::Random`
(`auth<github:skinkade>`, v0.4.1, Artistic-2.0) · **Kind:** Adopted (community
module, vendored as-is)

## What it is

Cryptographically secure random numbers and bytes mimicking `arc4random()`,
drawn from the OS entropy source (`/dev/urandom` on Unix, `CryptGenRandom()`
on Windows):

```raku
use Crypt::Random;
my Buf $bytes = crypt_random_buf(32);        # $n random bytes
my Int $n     = crypt_random();              # random positive Int (32-bit default)
my Int $u     = crypt_random_uniform(100);   # uniform in [0, 100), no modulo bias

use Crypt::Random::Extra;
my Str $uuid  = crypt_random_UUIDv4();       # random UUID, version/variant bits set
my Int $prime = crypt_random_prime(2048 div 8);
my @pick      = crypt_random_sample(@set, 3);
```

Four small files (~170 lines total); the OS-specific backend is selected at
`use` time via the `if` pragma (`use Crypt::Random::Nix:if(!$*DISTRO.is-win)`),
which mutsu implements natively — the ecosystem `if` dist is not needed.

## Why it is bundled

**It is a hard dependency of Cro::HTTP.** The Cro campaign
(`docs/batteries/web-framework.md`) needs every module in Cro::HTTP's `depends`
to load and work under mutsu; `Crypt::Random` supplies Cro's session-token and
boundary-string entropy. Bundling it (a) locks the working state in with the
release gate and (b) gives the bundle a proper CSPRNG slot — `rand`/`.pick` are
not safe for tokens or session ids, and the "small web blog" yardstick needs
exactly that.

**Selection.** The slot's winner is dictated by the Cro dependency edge — no
survey was run against alternatives (`Crypt::SysRandom` and friends) because a
substitute would not satisfy `Cro::HTTP`'s `depends` anyway. The module is the
de-facto ecosystem standard regardless (raku.land lists Cro::HTTP itself among
its dependents).

**Interpreter work it drove** (rung 2 — grow mutsu, never patch the module):

- `nqp::open` / `nqp::readfh` / `nqp::closefh` — the Nix backend reads
  `/dev/urandom` through these low-level ops (`src/runtime/nqp_ops.rs`;
  pin: `t/nqp-file-ops.t`).
- `:N[...]` radix lists flatten an embedded iterable — the UUID builder's
  `:256[$buf.values]` read the Seq as ONE digit and numified it to its element
  count (pin: `t/radix-list-iterable-flatten.t`).
- `Match.join` joins the positional captures (`.list`), not the matched
  string — the UUID's `(... ~~ /(........)(....).../).join("-")` came back
  dash-less (pin: `t/match-join-captures.t`).

Upstream tests: 3 files, 5 subtests — all pass under mutsu, matching raku
(`prove -e mutsu t/` in the upstream checkout). Smoke: `t/crypt-random-battery.t`.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `Crypt::Random` | <https://github.com/skinkade/crypt-random> | v0.4.1 | `c1bf9393` (2017-04-26) |

What is vendored: `lib/` plus `META6.json` and `README.md` for attribution
(upstream ships no separate LICENSE file; the README carries the license
statement). Upstream `t/` and CI config are excluded — the release gate fetches
the tests fresh at the pinned commit.

```sh
rsync -a --exclude '.precomp' <checkout>/lib/ modules/Crypt-Random/lib/
cp <checkout>/{META6.json,README.md} modules/Crypt-Random/
# then bump batteries.lock, re-run the gate, refresh the Pages manifest:
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump:

```sh
mutsu -e 'use Crypt::Random; say crypt_random_buf(16).elems'   # 16
mutsu -e 'use Crypt::Random::Extra; say crypt_random_UUIDv4()' # a well-formed UUID
```

Security updates ride the OS layer twice over: the entropy source itself is the
kernel's (`/dev/urandom`), and the Raku-level module is overridable via `mzef`
like every battery (BATTERIES.md §6).

## License

**Artistic-2.0** — stated in the upstream README ("This module may be used
under the terms of the Artistic License 2.0.", Copyright 2016 Shawn Kinkade).
`META6.json` carries no license key, so the Pages manifest pins it via the
sidecar map in `scripts/gen-batteries-manifest.py`. Vendored verbatim with
`META6.json` / `README` preserved for attribution, source unmodified (per
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
