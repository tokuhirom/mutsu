use Test;

# A closure created inside a `.map`/`.grep` block must keep *lexical* capture of
# an outer free variable even when the closure is later invoked through a callee
# that happens to have a same-named parameter. Previously the closure's free var
# degraded to dynamic scoping and resolved to the callee's parameter, because the
# inline map/grep fast path re-compiles the block body and the fresh copy lost
# the compile-time transitive "authoritative free var" propagation. This is
# exactly what zef's install pipeline hits: `Zef::Extract.extract` maps over
# backends and, inside `lock-file-protect(IO() $path, &code)` (whose parameter is
# named `$path`), runs `start { $extractor.extract($path, ...) }` where `$path`
# is the archive captured from the enclosing method — the leak made tar try to
# extract the `.lock` file.

plan 8;

sub callee($path, &code) { code() }

# --- map, one level of nesting ---
sub map1($path) { <A B>.map(-> $b { callee("$path.lock", -> { "$b:$path" }) }) }
is-deeply map1("REAL").Array, ["A:REAL", "B:REAL"], 'map block closure keeps outer $path (vs callee $path)';

# --- grep uses the same capture path ---
sub grep1($path) { (1, 2, 3).grep(-> $b { callee("X", -> { $path eq "REAL" }) }) }
is-deeply grep1("REAL").Array, [1, 2, 3], 'grep block closure keeps outer $path';

# --- List source (interpreter map path, not the native-array fast path) ---
is-deeply (1, 2).map(-> $b { callee("$_.lock", -> { "$b" }) }).Array,
    ["1", "2"], 'List.map inner closure runs (no leak)';

# --- deep cascade: map -> inner block -> start {} thread, 3 levels ---
sub deep($path) {
    <A B>.map(-> $b {
        callee("$path.lock", -> {
            my $todo = start { "$b:$path" };
            await $todo;
            $todo.result;
        });
    });
}
is-deeply deep("REAL").Array, ["A:REAL", "B:REAL"], 'cascade through inner block + start keeps $path';

# --- the non-map paths were always correct; guard them ---
sub direct($path) { my &blk = -> $b { callee("$path.lock", -> { "$b:$path" }) }; blk("A") }
is direct("REAL"), "A:REAL", 'direct closure call keeps $path';

sub forloop($path) { my @r; for <A B> -> $b { @r.push(callee("X", -> { $path })) }; @r }
is-deeply forloop("REAL").Array, ["REAL", "REAL"], 'for-loop closure keeps $path';

# --- no collision: still correct ---
sub nocollide($path) { <A>.map(-> $b { my $r; { my $lock = "L"; $r = "$b:$path"; }; $r }) }
is nocollide("REAL")[0], "A:REAL", 'map block reads outer $path directly';

# --- callee param renamed: no collision, must still work ---
sub callee2($lock, &code) { code() }
sub renamed($path) { <A>.map(-> $b { callee2("X", -> { "$b:$path" }) }) }
is renamed("REAL")[0], "A:REAL", 'no-collision callee still fine';
