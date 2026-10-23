use Test;

plan 4;

# The native callbacks the supply machinery hands to taps are compiled
# closures over their instance; a caller lexical that happens to share the
# internal capture name must not shadow it.
my $native_shim_instance = "caller's own value";
my $v = "caller v";

my @lines;
supply { emit "a\nb\n" }.lines.tap({ @lines.push: $_ });
is @lines.join(","), "a,b", 'derived .lines supply runs with a caller lexical of the capture name';

my @mapped;
supply { emit 1; emit 2 }.map(* * 10).tap({ @mapped.push: $_ });
is @mapped.join(","), "10,20", 'derived .map supply is unaffected';

my $done = False;
supply { emit 1; done }.tap(-> $x { }, done => { $done = True });
ok $done, 'done callback shim runs';

is $native_shim_instance, "caller's own value", 'the caller lexical is untouched';
