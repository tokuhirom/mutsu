use Test;

plan 3;

my $dir = $*TMPDIR.child("mutsu-compunit-loader-{$*PID}");
$dir.mkdir;
END { try { .unlink for $dir.dir; $dir.rmdir } }

my $source = $dir.child('helper.raku');
$source.spurt('our $value = "loaded"; sub MAIN(Str $name) { say "hello $name"; }');
my $plain-source = $dir.child('plain.raku');
$plain-source.spurt('say "hello";');

my $captured = '';
my $old-out = $*OUT;
$*OUT = class {
    method print(*@args) { $captured ~= @args.join }
    method flush { }
};
@*ARGS = ["world"];
my $handle = CompUnit::Loader.load-source-file($source);
$*OUT = $old-out;

is $captured, "hello world\n", 'load-source-file dispatches MAIN with the caller arguments';

my %vars = $handle.globalish-package;
is %vars{'$value'}, 'loaded', 'the handle exposes the loaded globalish package';

module Test::LoaderProbe {
    our sub capture(Str $path) is export {
        my $output;
        my $saved = $*OUT;
        $*OUT = class {
            method print(*@args) { $output ~= @args.join }
            method flush { }
        };
        CompUnit::Loader.load-source-file($path.IO);
        $*OUT = $saved;
        $output
    }
}

is Test::LoaderProbe::capture($plain-source.Str), "hello\n",
    'a loader call from a nested Test package keeps its local output capture';
