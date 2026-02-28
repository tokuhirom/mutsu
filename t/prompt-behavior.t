use Test;

plan 4;

my $in-file  = "prompt-behavior-in.txt";
my $out-file = "prompt-behavior-out.txt";
spurt $in-file, "foobar\n";
END {
    unlink $in-file;
    unlink $out-file;
}

{
    my $out = $out-file.IO.open: :w, :nl-out<MEOW>;
    temp $*OUT = $out;
    temp $*IN = $in-file.IO.open: :!chomp, :nl-in<oba>;
    is prompt("X"), "fooba", 'prompt respects :!chomp and :nl-in on input handle';
    $out.close;
}
is slurp($out-file), "X", 'prompt writes message without newline';

{
    temp $*IN = $in-file.IO.open;
    my $out = $out-file.IO.open: :w;
    temp $*OUT = $out;
    my $msg = class { method Str { "pass" } }.new;
    is prompt($msg), "foobar", 'prompt returns a line from input';
    $out.close;
    is slurp($out-file), "pass", 'prompt stringifies message via .Str';
}
