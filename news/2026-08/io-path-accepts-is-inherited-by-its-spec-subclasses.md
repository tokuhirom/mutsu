# `IO::Path.ACCEPTS` is inherited by the SPEC-variant subclasses

Rakudo declares `ACCEPTS` on `IO::Path`, so `IO::Path::Unix`, `::Win32`,
`::Cygwin` and `::QNX` inherit it. mutsu's smartmatch arms — both the pure
value-level one (`vm/vm_smart_match.rs`) and the interpreter-level one
(`runtime/seq_helpers/smart_match.rs`) — guarded on the *exact* class name
`IO::Path`, so any comparison involving a subclass instance fell through to the
generic instance arm and answered `False`:

```raku
my $a = IO::Path::Unix.new('/foo/').add('bar');
my $b = IO::Path::Unix.new('/foo/bar');
say $a.resolve.raku eq $b.resolve.raku;   # True in both
say $a.resolve ~~ $b.resolve;             # raku: True   mutsu: False
```

Four arms in each file were affected: `IO::Path ~~ IO::Path`, `Cool ~~
IO::Path`, `IO::Path ~~ Str`, and the file-test adverbs (`$path ~~ :e`, `:d`,
`:f`, …), which are candidates on `IO::Path` too. All eight now test membership
of the built-in family through the existing
`Interpreter::is_io_path_lexical_class` predicate, which is the same set the
native path-method fast path already uses.

The concrete consumer is `Test::Util`'s `is-path`, which is
`cmp-ok $got.resolve, '~~', $exp.resolve, $desc` — under the real Test::Util
module (as opposed to mutsu's native override of it) that assertion could never
pass. It was one of the two blockers listed in
`todo/tickets/retire-native-test-util-overrides.md`.

Pin: `t/io-path-accepts-subclass.t`, which asserts the same ten comparisons
under `raku` and mutsu.
