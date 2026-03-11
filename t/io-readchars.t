use Test;

plan 7;

my $file = "tmp-io-readchars-{$*PID}.txt";
my $dir = "tmp-io-readchars-dir-{$*PID}";

spurt $file, "a♥c";
my $fh = open($file);
is-deeply ($fh.readchars(1) xx 4).list, ("a", "♥", "c", ""), "readchars(1) returns full UTF-8 characters";
$fh.close;

$fh = open($file);
is $fh.readchars(2), "a♥", "readchars(2) counts characters, not bytes";
is $fh.readchars, "c", "readchars with no count reads remaining characters";
$fh.close;

ok mkdir($dir), "temporary directory created";
dies-ok { open($dir).readchars }, "readchars on a directory fails";

ok unlink($file), "temporary file removed";
ok rmdir($dir), "temporary directory removed";
