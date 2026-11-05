use v6.d;
use Test;

# `$*OUT = $handle` after `my $*OUT = ...` redirects `say` / `put` / `print`
# to that handle, like the declaration form does (#15647).

plan 5;

class Collector is IO::Handle {
    has @.got;
    method WRITE(Blob:D \data --> Bool:D) { @!got.push: data.decode; True }
}
sub capture { my $h = Collector.new(:!chomp); $h.encoding('utf8'); $h }

my $decl = capture;
{
    my $*OUT = $decl;
    say "a"; put "b"; print "c";
}
is $decl.got.join, "a\nb\nc", 'declared with the handle';

my $assigned = capture;
{
    my $*OUT = $*ERR;
    $*OUT = $assigned;
    say "a"; put "b"; print "c";
}
is $assigned.got.join, "a\nb\nc", 'assigned after the declaration';

my $later = capture;
{
    my $*OUT;
    $*OUT = $later;
    "x".put;
}
is $later.got.join, "x\n", 'assigned to an undeclared-value dynamic';

my $in-sub = capture;
sub redirect { my $*OUT = $*ERR; $*OUT = $in-sub; say "in-sub"; }
redirect();
is $in-sub.got.join, "in-sub\n", 'assigned inside a routine';

my $callee = capture;
sub says { say "callee" }
sub wrap { my $*OUT = $*ERR; $*OUT = $callee; says(); }
wrap();
is $callee.got.join, "callee\n", 'a callee writes to the assigned handle';
