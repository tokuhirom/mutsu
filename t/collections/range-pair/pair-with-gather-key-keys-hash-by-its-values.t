use v6;
use Test;

plan 6;

my %g = (gather { take "k" }) => 3;
is-deeply %g, {k => 3}, "a gather Seq key keys the hash by its element";

my $s = gather { take "a"; take "b" };
my %h = $s => 1;
is-deeply %h, {"a b" => 1}, "a multi-element gather key joins with a space";

my $p = (gather { take "z" }) => 7;
is %($p).keys.join, "z", "%($p) keys it the same way";

class C {
    has @.args = (1,);
    method col { "bla-id" }
    method find-name { gather for self.args { .take for self.?col } }
    method should-set(--> Hash()) { self.find-name => 1 }
}
is-deeply C.new.should-set, {"bla-id" => 1}, "a gather-built key inside a --> Hash() method";

my %e = ("k", "j").Seq => 1;
is-deeply %e, {"k j" => 1}, "a plain Seq key is unchanged";
is-deeply (gather { take "q" }).Str, "q", "gather.Str is unchanged";
