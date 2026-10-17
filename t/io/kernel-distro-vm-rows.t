use Test;

# The `Systemic` classes' methods are rows (ADR-11276 §9.155): `Distro`,
# `Kernel`, `VM`, `Raku` and `Compiler`, the objects behind `$*DISTRO`,
# `$*KERNEL`, `$*VM`, `$*RAKU` and `$*RAKU.compiler`.

plan 20;

isa-ok $*DISTRO.name, Str, 'Distro.name';
isa-ok $*DISTRO.is-win, Bool, 'Distro.is-win';
is $*DISTRO.Str, $*DISTRO.name, 'Distro.Str is its name';
like $*DISTRO.gist, /^ \S+ ' (' .* ')' $/, 'Distro.gist is name (version)';
like $*DISTRO.raku, /^ 'Distro.new(release => '/, 'Distro.raku is Mu.raku of its attributes';

isa-ok $*KERNEL.bits, Int, 'Kernel.bits';
isa-ok $*KERNEL.cpu-cores, Int, 'Kernel.cpu-cores';
isa-ok Kernel.cpu-cores, Int, 'Kernel.cpu-cores on the type object';
is $*KERNEL.signal('HUP'), 1, 'Kernel.signal of a name';
is $*KERNEL.Str, $*KERNEL.name, 'Kernel.Str is its name';

isa-ok $*VM.name, Str, 'VM.name';
like $*VM.raku, /^ 'VM.new(config => '/, 'VM.raku is Mu.raku of its attributes';
like ~$*VM.platform-library-name('foo'.IO), /foo/, 'VM.platform-library-name';
dies-ok { $*VM.properties }, 'VM declares no properties';

isa-ok $*RAKU.compiler, Compiler, 'Raku.compiler';
is $*RAKU.Str, 'Raku', 'Raku.Str';
like $*RAKU.raku, /^ 'Raku.new(' /, 'Raku.raku';

isa-ok Compiler.id, Str, 'Compiler.id on the type object';
isa-ok $*RAKU.compiler.verbose-config, Hash, 'Compiler.verbose-config';
like $*RAKU.compiler.raku, /^ 'Compiler.new(id => '/, 'Compiler.raku';
