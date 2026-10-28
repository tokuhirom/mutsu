use Test;

# A module's `sub EXPORT` runs in the module's own lexical scope, so it sees
# what the module itself imported -- on EVERY import, including a re-import
# from a module that is itself being loaded (its scope holds none of the
# provider's candidates). NativeLibs' hook introspects NativeCall's
# `trait_mod:<is>` candidates this way; Red 0.2.7's `require`d SQLite driver
# chain re-imports it from DBDish::SQLite::Native.

plan 3;

my $dir = $*TMPDIR.add("mutsu-export-hook-" ~ $*PID ~ "-" ~ (^1000000).pick);
LEAVE { try { .unlink for $dir.add('Mid').dir; $dir.add('Mid').rmdir; .unlink for $dir.dir; $dir.rmdir } }
$dir.add('Mid').mkdir;

$dir.add('Prov.rakumod').spurt: q:to/MOD/;
    unit module Prov;
    multi trait_mod:<is>(Mu:U $m, :$prov-flag!) is export { }
    MOD
$dir.add('Hook.rakumod').spurt: q:to/MOD/;
    use Prov;
    sub EXPORT(|) {
        my $n = &trait_mod:<is>.candidates.grep({ .signature ~~ :(Mu:U, :$prov-flag!) }).elems;
        die "hook sees no Prov candidate" unless $n;
        Map.new('&hook-count' => sub { $n })
    }
    unit module Hook;
    MOD
$dir.add('Mid.rakumod').spurt: q:to/MOD/;
    unit class Mid:ver<0.1>;
    use Hook;
    use Mid::Native;
    method count { hook-count() ~ '/' ~ Mid::Native::native-count() }
    MOD
$dir.add('Mid/Native.rakumod').spurt: q:to/MOD/;
    use Hook;
    unit module Mid::Native;
    our sub native-count is export { hook-count() }
    MOD

my $out = EVAL qq:to/CODE/;
    use lib '$dir';
    \{
        require ::("Mid");
        ::("Mid").new.count;
    \}
    CODE
is $out, '1/1', 'the hook found Prov\'s candidate on the first import and on the nested re-import';

is EVAL("use lib '$dir'; use Hook; hook-count()"), 1, 'and on a plain script import';
lives-ok { EVAL("use lib '$dir'; use Hook; use Hook; 1") }, 'and on a repeated import';
