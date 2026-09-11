"""Shared machinery for the ecosystem sweeps.

Two tools run real, unaudited ecosystem code under mutsu:

* `dist-compat-sweep.py` — the sampling diagnostic ("does `use <module>` work?"),
  bucketing load failures by root cause for `TODO_dist/TICKETS.md`;
* `ecosystem-sweep.py` — the exhaustive parity ledger (ADR-0085): every
  distribution's own test suite, run on both rakudo and mutsu.

They share the index reader, the tarball cache, the sandbox wrapper and the TAP
parser, so the security-relevant code (the `bwrap` confinement above all) exists
in one place rather than in two copies that drift.

See docs/ecosystem-parity.md and docs/dist-compat-sweep.md.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tarfile
import urllib.parse
import urllib.request

# --- ecosystem indexes -------------------------------------------------------

FEZ_URL = "https://360.zef.pm/"
FEZ_CDN = "https://360.zef.pm/"
FEZ_LOCAL = os.path.expanduser("~/.zef/store/fez/fez.json")
# The Raku Ecosystem Archive: the legacy p6c/CPAN distributions that never moved
# to fez. Without it a quarter of the corpus has an unsatisfiable dependency
# closure -- see the coverage table in docs/ecosystem-parity.md section 3.
REA_URL = "https://raw.githubusercontent.com/Raku/REA/main/META.json"

CACHE_DIR = os.path.expanduser("~/.cache/mutsu-ecosystem")

# Names the compiler supplies; never a distribution to resolve.
CORE_PROVIDED = {
    "Test", "NativeCall", "nqp", "Telemetry", "MONKEY", "MONKEY-SEE-NO-EVAL",
    "experimental", "lib", "Pod::To::Text",
}

_DEP_NAME = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*(?:::[A-Za-z0-9_]+)*)")
_TEST_FILE = re.compile(r"\.(t|rakutest)$")
_SOURCE_FILE = re.compile(r"\.(rakumod|pm6|pm|raku|rakutest|t)$")

# Compiler-guts and NativeCall signals. Neither changes a verdict -- a
# distribution is measured either way -- but recording which axis a failure sits
# on keeps "needs an nqp:: op" from being triaged as an ordinary interpreter bug.
GUTS_SIGNAL = re.compile(
    r"QAST|Metamodel::Primitives|NQPHLL|:from<NQP>|EXPORTHOW|define_slang|"
    r"package_declarator:sym|use\s+experimental\s*:\s*macro|Slang::|\buse\s+nqp\b"
)
NATIVE_SIGNAL = re.compile(r"\buse\s+NativeCall\b|:from<C>|\bis\s+native\b")


def source_axis(root: str) -> str:
    """'guts' / 'native' / 'pure', from the distribution's own sources."""
    text = []
    for dirpath, _dirs, names in os.walk(root):
        for name in names:
            if not _SOURCE_FILE.search(name):
                continue
            try:
                with open(os.path.join(dirpath, name), encoding="utf8", errors="ignore") as fh:
                    text.append(fh.read())
            except OSError:
                pass
    joined = "".join(text)
    if GUTS_SIGNAL.search(joined):
        return "guts"
    if NATIVE_SIGNAL.search(joined):
        return "native"
    return "pure"


def version_key(v):
    return [(0, int(x)) if x.isdigit() else (1, x) for x in re.split(r"[.\-+]", str(v))]


def dep_names(meta: dict, *, include_test: bool = True) -> list[str]:
    """Every dependency name in a META6 entry, flattened.

    `depends` is allowed to be a plain list or the nested
    `{runtime: {requires: [...]}}` form, and entries carry adverbs
    (`Foo::Bar:ver<1.2>:auth<zef:x>`) or, occasionally, prose. Returns bare
    names; `:from<native>` / `:from<bin>` entries are dropped (they are C
    libraries and executables, not distributions).
    """
    out: list[str] = []

    def walk(node):
        if isinstance(node, list):
            out.extend(x for x in node if isinstance(x, str))
        elif isinstance(node, dict):
            for value in node.values():
                walk(value)

    walk(meta.get("depends") or [])
    if include_test:
        walk(meta.get("test-depends") or [])

    names = []
    for raw in out:
        if ":from<native>" in raw or ":from<bin>" in raw:
            continue
        m = _DEP_NAME.match(raw.strip())
        if m:
            names.append(m.group(1))
    return names


def _entry_url(entry: dict) -> str | None:
    """Normalize the two index shapes to one fetch URL: fez carries a `path`
    relative to its CDN, REA a full `source-url`."""
    if entry.get("path"):
        return FEZ_CDN + entry["path"]
    return entry.get("source-url")


def _fetch_json(url: str, cache_path: str, refresh: bool):
    if refresh or not os.path.exists(cache_path):
        os.makedirs(os.path.dirname(cache_path), exist_ok=True)
        with urllib.request.urlopen(url, timeout=180) as response:
            data = response.read()
        with open(cache_path, "wb") as fh:
            fh.write(data)
    with open(cache_path, "rb") as fh:
        raw = fh.read()
    return json.loads(raw), hashlib.sha256(raw).hexdigest()


def load_fez_index(path: str = FEZ_LOCAL) -> dict:
    """The local fez index zef/mzef maintain, latest version of each dist.

    This is what `dist-compat-sweep.py` has always read; it is kept as its own
    entry point so that tool's behaviour does not change.
    """
    if not os.path.exists(path):
        sys.exit(f"missing {path} — run an mzef/zef ecosystem op first")
    with open(path, encoding="utf-8") as fh:
        entries = json.load(fh)
    return _latest_by_name(entries)


def _latest_by_name(entries, into: dict | None = None) -> dict:
    best = {} if into is None else into
    for entry in entries:
        if not isinstance(entry, dict) or not entry.get("name"):
            continue
        if not _entry_url(entry):
            continue
        name = entry["name"]
        if name not in best or version_key(entry.get("version", "0")) > version_key(
            best[name].get("version", "0")
        ):
            best[name] = entry
    return best


class Index:
    """The merged fez + REA ecosystem index.

    `targets` are the fez distributions (what gets measured); `dists` is the
    merged pool used to satisfy dependencies, so a legacy REA-only dependency
    resolves without becoming a measurement target itself.
    """

    def __init__(self, targets: dict, dists: dict, provides: dict, snapshot: dict):
        self.targets = targets
        self.dists = dists
        self.provides = provides
        self.snapshot = snapshot

    def resolve_name(self, name: str) -> str | None:
        """A dependency name to a distribution name: it may be the dist itself,
        or a module some other dist `provides`."""
        if name in self.dists:
            return name
        return self.provides.get(name)

    def closure(self, dist: str, *, extra_supplied: set[str] | None = None):
        """Transitive dependency closure of `dist`, excluding `dist`.

        Returns `(resolved, unresolved)`. `extra_supplied` names are treated as
        already available (used to ask, separately, what mutsu's bundled
        batteries would have supplied — never to widen the closure itself; see
        ADR-0085 D5).
        """
        supplied = extra_supplied or set()
        seen = {dist}
        resolved: set[str] = set()
        unresolved: set[str] = set()
        stack = [dist]
        while stack:
            current = stack.pop()
            entry = self.dists.get(current)
            if entry is None:
                continue
            for name in dep_names(entry):
                if name in CORE_PROVIDED or name in supplied:
                    continue
                target = self.resolve_name(name)
                if target is None:
                    unresolved.add(name)
                elif target not in seen:
                    seen.add(target)
                    resolved.add(target)
                    stack.append(target)
        return sorted(resolved), sorted(unresolved)

    def url(self, dist: str) -> str | None:
        entry = self.dists.get(dist)
        return _entry_url(entry) if entry else None


def load_index(cache_dir: str = CACHE_DIR, *, refresh: bool = False,
               with_rea: bool = True) -> Index:
    """Fetch (or reuse) both indexes and merge them, highest version winning."""
    fez, fez_digest = _fetch_json(FEZ_URL, os.path.join(cache_dir, "index", "fez.json"), refresh)
    targets = _latest_by_name(fez)
    dists = dict(targets)
    snapshot = {"fez": {"url": FEZ_URL, "sha256": fez_digest, "dists": len(targets)}}
    if with_rea:
        rea, rea_digest = _fetch_json(REA_URL, os.path.join(cache_dir, "index", "rea.json"), refresh)
        before = len(dists)
        _latest_by_name(rea, dists)
        snapshot["rea"] = {"url": REA_URL, "sha256": rea_digest,
                           "dists_added": len(dists) - before}
    provides: dict[str, str] = {}
    for name, entry in dists.items():
        for module in (entry.get("provides") or {}):
            provides.setdefault(module, name)
    snapshot["modules"] = len(provides)
    return Index(targets, dists, provides, snapshot)


# --- tarball cache -----------------------------------------------------------

def fetch_tarball(url: str, cache_dir: str) -> str:
    """Download `url` once into `cache_dir` and return the local path."""
    os.makedirs(cache_dir, exist_ok=True)
    name = urllib.parse.unquote(urllib.parse.urlparse(url).path).lstrip("/")
    safe = re.sub(r"[^A-Za-z0-9._-]", "_", name)[-160:]
    # A name alone can collide once two indexes are merged; the digest of the
    # full URL keeps two same-named tarballs from sharing a cache slot.
    cache_path = os.path.join(cache_dir, f"{hashlib.sha256(url.encode()).hexdigest()[:12]}-{safe}")
    if not os.path.exists(cache_path):
        with urllib.request.urlopen(url, timeout=120) as response:
            data = response.read()
        tmp = cache_path + ".part"
        with open(tmp, "wb") as fh:
            fh.write(data)
        os.replace(tmp, cache_path)
    return cache_path


def extract_dist(tar_path: str, dest: str) -> str | None:
    """Extract into `dest` and return the distribution root (the directory
    holding META6.json), or None when there is no META6.json."""
    os.makedirs(dest, exist_ok=True)
    with tarfile.open(tar_path) as tf:
        tf.extractall(dest, filter="data")
    if os.path.exists(os.path.join(dest, "META6.json")):
        return dest
    subdirs = [os.path.join(dest, e) for e in os.listdir(dest)]
    subdirs = [d for d in subdirs if os.path.isdir(d)]
    if len(subdirs) == 1 and os.path.exists(os.path.join(subdirs[0], "META6.json")):
        return subdirs[0]
    for d in subdirs:
        if os.path.exists(os.path.join(d, "META6.json")):
            return d
    return None


def read_meta6(root: str) -> dict | None:
    path = os.path.join(root, "META6.json")
    try:
        with open(path, encoding="utf-8", errors="replace") as fh:
            return json.load(fh)
    except Exception:
        return None


def find_test_files(root: str, *, include_xt: bool = False) -> list[str]:
    """Test files under the distribution's test directories.

    `xt/` is author tooling (style, pod, META lint) that tests the distribution
    rather than the language, and is what `zef test` leaves alone; it is
    excluded unless asked for.
    """
    subdirs = ["t", "test"] + (["xt"] if include_xt else [])
    files = []
    for sub in subdirs:
        directory = os.path.join(root, sub)
        if not os.path.isdir(directory):
            continue
        for dirpath, _dirs, names in os.walk(directory):
            files.extend(os.path.join(dirpath, n) for n in names if _TEST_FILE.search(n))
    return sorted(files)


# --- sandbox -----------------------------------------------------------------

def have_bwrap() -> bool:
    return shutil.which("bwrap") is not None


def sandbox_wrap(cmd, root, sbx_home, mem_kb=6_000_000, nproc=400, *, writable=()):
    """Wrap `cmd` so untrusted distribution code runs with NO network, a
    read-only filesystem, an isolated throwaway HOME, its own PID namespace,
    and rlimits. `sbx_home` must already exist (a fresh tmpfs is mounted over
    it, so nothing the code writes reaches the host).

    Loading a module, let alone running its test suite, executes arbitrary
    BEGIN/CHECK phasers and load-time code from an unaudited distribution.
    Downloading and extracting happen OUTSIDE the sandbox; only the interpreter
    run is confined, and it needs no network.

    `writable` re-binds specific paths read-write over the read-only root. The
    parity sweep passes the throwaway copy of the distribution under test: real
    suites write scratch files next to their fixtures, and a read-only cwd would
    turn that into a failure on both interpreters -- symmetric, so not a false
    regression, but it would silently shrink the measurable baseline. Only pass
    a path you are willing to see destroyed.
    """
    binds = []
    for path in writable:
        binds += ["--bind", path, path]
    return [
        "bwrap",
        "--unshare-all",              # user+net+pid+ipc+uts+cgroup+mount (net = offline)
        "--ro-bind", "/", "/",        # whole rootfs, read-only
        *binds,
        "--dev", "/dev",
        "--proc", "/proc",
        "--tmpfs", "/run",
        "--tmpfs", sbx_home,          # writable throwaway HOME (tmpfs over an existing dir)
        "--setenv", "HOME", sbx_home,
        "--chdir", root,
        "--die-with-parent",
        "--new-session",
        "bash", "-c", f"ulimit -v {mem_kb} -u {nproc} 2>/dev/null; exec \"$@\"", "_",
    ] + cmd


# --- TAP ---------------------------------------------------------------------

PLAN_RE = re.compile(r"^1\.\.(\d+)\s*$", re.M)
OK_RE = re.compile(r"^ok\b", re.M)
NOTOK_RE = re.compile(r"^not ok\b")
SKIP_RE = re.compile(r"^ok\b.*#\s*skip", re.I)

_HARNESS_NOISE = re.compile(
    r"test failures|you planned|you failed|looks like you|^#|^1\.\.|dubious|"
    r"^ok\b|^not ok\b", re.I)


def parse_tap(out: str):
    """(planned, ok, real_not_ok, todo_not_ok, skipped) from raw TAP output.

    A `not ok N - desc # TODO reason` line is an *expected* failure: TAP says
    the file still passes and `prove` agrees, so it is counted apart from a
    real failure rather than against it.
    """
    m = PLAN_RE.search(out)
    plan = int(m.group(1)) if m else None
    ok = len(OK_RE.findall(out))
    real_notok = todo = skip = 0
    for line in out.splitlines():
        if NOTOK_RE.match(line):
            if "todo" in line.lower():
                todo += 1
            else:
                real_notok += 1
        elif SKIP_RE.match(line):
            skip += 1
    return plan, ok, real_notok, todo, skip


def tap_verdict(out: str, rc):
    """'pass' / 'fail' / 'die'.

    'die' = crashed before or mid-TAP (no plan, ran fewer than planned, or a
    non-zero exit with no failing assertion to blame). 'fail' = a real `not ok`.
    """
    plan, ok, notok, todo, _skip = parse_tap(out)
    if plan is None:
        return "die"
    if notok > 0:
        return "fail"
    if ok + notok + todo < plan:
        return "die"
    if rc not in (0, None):
        return "die"
    return "pass"


def first_error_line(out: str) -> str:
    """The first meaningful error line, skipping the generic TAP-harness noise
    ('Runtime error: Test failures', '# You planned N ...') that reports only
    that the run died, not why.

    A `===SORRY!=== Error while compiling <path>` header is noise of the same
    kind: it says a parse failed and then spends its width on a temp-directory
    path. The reason is the line after it, which is what a reader needs to tell
    one parse failure from another -- without this, every parse blocker in the
    ledger reads as the same undifferentiated `===SORRY!===`.
    """
    candidates = []
    for line in out.splitlines():
        s = line.strip()
        if not s or _HARNESS_NOISE.search(s) or s.startswith("===SORRY!==="):
            continue
        candidates.append(s)
    for s in candidates:
        low = s.lower()
        if ("sorry" in low or "panicked" in low or "unhandled" in low
                or s.startswith("X::") or ("::" in s and "exception" in low)
                or "no such" in low or "unknown method" in low
                or "unknown function" in low or "cannot" in low
                or low.startswith("runtime error")):
            return s[:200]
    return candidates[0][:200] if candidates else ""


def first_failing_assertion(out: str) -> str:
    """The description of the first real (non-TODO) `not ok` line."""
    for line in out.splitlines():
        if NOTOK_RE.match(line) and "todo" not in line.lower():
            m = re.match(r"not ok\s+\d+\s*-?\s*(.*)", line.strip())
            desc = (m.group(1).strip() if m else line.strip())
            return desc[:200] if desc else line.strip()[:200]
    return first_error_line(out)


# --- misc --------------------------------------------------------------------

def rmtree(path: str) -> None:
    subprocess.run(["rm", "-rf", path], check=False)


# --- record filenames --------------------------------------------------------

# Characters `actions/upload-artifact` refuses in a path, because NTFS cannot
# hold them, plus the path separators and `%` (which the escape below uses, so
# escaping it keeps that part of the mapping injective).
#
# This list is load-bearing, not defensive tidiness: ONE rejected path fails the
# WHOLE artifact upload. `App:Racl` and `Slang:Date` -- two fez distributions
# whose names carry a single colon, which the `::` -> `--` rule does not touch --
# cost shards A and S every record they had measured, twenty minutes each, on the
# first corpus sweep.
_UNSAFE_IN_FILENAME = re.compile(r'[:"<>|*?%/\\\r\n]')

# How many hex digits of the name's digest go in the filename. It only has to
# separate names that would otherwise share a stem, and the corpus has exactly
# two such pairs, so 8 is far more than enough.
_DIGEST_LEN = 8


def record_filename(dist: str) -> str:
    """The `ecosystem/dists/<S>/` filename for a distribution.

    `String::Utils` becomes `String--Utils~<digest>.json`. Three properties, each
    of which the corpus actually needs:

    * **Readable.** `::` becomes `--`, so the stem is the distribution name.
    * **Legal everywhere.** Anything a filesystem or a GitHub artifact upload
      would reject becomes `%XX`, so `App:Racl` is `App%3ARacl~...json`. One
      rejected path fails an entire artifact upload.
    * **Injective, including case-insensitively.** This is why the digest is
      there, and it is not paranoia -- `::` -> `--` is NOT injective over the
      real index, and a case-insensitive filesystem collapses more still:

        | these distinct distributions | shared this filename |
        |---|---|
        | `Qwiratry::Location::HTTP`, `Qwiratry--Location--HTTP` | exactly |
        | `WWW::CloudHosting::Hetzner`, `WWW--CloudHosting--Hetzner` | exactly |
        | `CSV-AutoClass`, `CSV-Autoclass` | on a case-insensitive filesystem |
        | `Config::INI`, `Config::Ini` | on a case-insensitive filesystem |

      The first two silently overwrote each other *in the ledger*; the last two
      were dropped by the artifact upload, which is case-insensitive, and would
      break a macOS or Windows checkout. A digest over the exact name separates
      all four pairs and cannot rot as the index grows -- unlike a
      disambiguate-only-when-needed rule, which would rename an existing record
      the day a colliding distribution is published.
    """
    stem = _UNSAFE_IN_FILENAME.sub(lambda m: "%%%02X" % ord(m.group()),
                                   dist.replace("::", "--"))
    digest = hashlib.sha256(dist.encode("utf-8")).hexdigest()[:_DIGEST_LEN]
    return f"{stem}~{digest}.json"


# --- self-test ---------------------------------------------------------------

def _self_test() -> int:
    """`python3 scripts/ecosystem_common.py --self-test`

    The failure-line extractor decides what every blocked distribution in the
    ledger *says*, so a regression there does not break a sweep -- it quietly
    makes thirty distinct blockers read as one, which is how the first sweep's
    parse failures were nearly triaged as a single cause.
    """
    cases = [
        # A ===SORRY!=== header spends its width on a temp path; the reason is
        # the line after it.
        ("===SORRY!=== Error while compiling /tmp/x/lib/A.rakumod\n"
         "expected statement: expected ')'\n"
         "at lib/A.rakumod:50",
         "expected statement: expected ')'"),
        # Generic TAP-harness death lines report that a run died, not why.
        ("1..3\nok 1 - a\nRuntime error: Test failures\n"
         "No such method 'frobnicate' for invocant of type 'Str'",
         "No such method 'frobnicate' for invocant of type 'Str'"),
        ("Unknown role: CustomUnmarshaller", "Unknown role: CustomUnmarshaller"),
    ]
    failures = 0
    for out, want in cases:
        got = first_error_line(out)
        if got != want:
            print(f"first_error_line: want {want!r}, got {got!r}", file=sys.stderr)
            failures += 1

    # A `# TODO` failure is an expected one: TAP says the file still passes.
    tap = "1..3\nok 1 - a\nnot ok 2 - b # TODO flaky\nok 3 - c\n"
    if parse_tap(tap) != (3, 2, 0, 1, 0):
        print(f"parse_tap: got {parse_tap(tap)}", file=sys.stderr)
        failures += 1
    for out, rc, want in [
        (tap, 0, "pass"),
        ("1..2\nok 1\nnot ok 2 - boom\n", 1, "fail"),
        ("1..3\nok 1\n", 0, "die"),          # ran fewer than planned
        ("no plan at all\n", 0, "die"),
    ]:
        got = tap_verdict(out, rc)
        if got != want:
            print(f"tap_verdict: want {want}, got {got} for {out!r}", file=sys.stderr)
            failures += 1

    # The stem must stay readable, and every character an artifact upload
    # rejects must be gone. The digest is checked for its own property below.
    for dist, want_stem in [
        ("String::Utils", "String--Utils"),
        ("BTree", "BTree"),
        # The two real fez names whose single colon cost shards A and S their
        # measurements on the first corpus sweep.
        ("App:Racl", "App%3ARacl"),
        ("Slang:Date", "Slang%3ADate"),
        # Every other rejected character, and the escape's own.
        ('A"B', "A%22B"), ("A<B>C", "A%3CB%3EC"), ("A|B", "A%7CB"),
        ("A*B", "A%2AB"), ("A?B", "A%3FB"), ("A%B", "A%25B"),
        ("A/B", "A%2FB"), ("A\\B", "A%5CB"),
    ]:
        got = record_filename(dist)
        if not re.fullmatch(re.escape(want_stem) + r"~[0-9a-f]{8}\.json", got):
            print(f"record_filename({dist!r}): want {want_stem}~<8 hex>.json, "
                  f"got {got!r}", file=sys.stderr)
            failures += 1
        if re.search(r'[:"<>|*?\r\n]', got):
            print(f"record_filename({dist!r}) still artifact-hostile: {got!r}", file=sys.stderr)
            failures += 1

    # Injectivity is the property the ledger depends on, and these four pairs are
    # real: the first two collide under `::` -> `--` alone, the last two collide
    # on any case-insensitive filesystem (which is what an artifact upload is).
    for a, b in [
        ("Qwiratry::Location::HTTP", "Qwiratry--Location--HTTP"),
        ("WWW::CloudHosting::Hetzner", "WWW--CloudHosting--Hetzner"),
        ("CSV-AutoClass", "CSV-Autoclass"),
        ("Config::INI", "Config::Ini"),
    ]:
        fa, fb = record_filename(a), record_filename(b)
        if fa == fb:
            print(f"record_filename collides: {a!r} and {b!r} both -> {fa!r}",
                  file=sys.stderr)
            failures += 1
        if fa.lower() == fb.lower():
            print(f"record_filename collides case-insensitively: {a!r} and {b!r} "
                  f"-> {fa!r} / {fb!r}", file=sys.stderr)
            failures += 1
    # And it is stable: the same name always gets the same filename.
    if record_filename("String::Utils") != record_filename("String::Utils"):
        print("record_filename is not deterministic", file=sys.stderr)
        failures += 1

    print(f"ecosystem_common self-test: "
          f"{'all cases pass' if not failures else f'{failures} failure(s)'}")
    return 1 if failures else 0


if __name__ == "__main__":
    if "--self-test" in sys.argv:
        raise SystemExit(_self_test())
    sys.exit("this module is imported by the sweeps; --self-test runs its checks")
