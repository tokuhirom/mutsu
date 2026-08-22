# `IO::Path::Win32` inconsistently normalizes `/` vs `\` separators

Discovered via the doc-diff harness on `raku-doc/doc/Type/IO/Path.rakudoc` (around lines 128,
290, 423).

## Repros

```
IO::Path::Win32.new('//server/share').basename.say;
```
- raku: `\`
- mutsu: `/`

```
IO::Path::Win32.new("C:\foo/bar\").say;
```
- raku: round-trips the trailing `\` — `"C:\foo/bar\".IO`
- mutsu: normalizes it to `"C:\foo\bar".IO` (drops/changes the trailing separator)

```
IO::Path::Win32.new('C:/').parent.say;
```
- raku: `"C:/".IO` (preserves the `/` the caller used)
- mutsu: `"C:\".IO` (normalizes to `\`)

## Root cause guess

`IO::Path::Win32`'s basename/parent/`.Str` computation doesn't consistently thread the Win32
`SPEC`'s separator preference through every code path — some operations hardcode `/`, others
normalize to `\`, and raku's actual behavior is to largely preserve whichever separator the
caller originally used rather than aggressively normalizing.

## Affected files (starting point)

- `src/runtime/native_io/path_spec.rs` — `io_path_parts`, `io_path_sep`, basename/dirname/parent
  computation for the Win32 spec

## Suggested next step

Read `raku-doc/doc/Type/IO/Spec/Win32.rakudoc` (also flagged with divergences in the backlog
survey) for the documented separator/normalization rules, then audit every place
`path_spec.rs` hardcodes `'/'` or `'\\'` instead of consulting the path's original separator or
the spec's canonical one consistently.
