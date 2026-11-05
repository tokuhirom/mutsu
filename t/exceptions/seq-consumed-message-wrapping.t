use Test;
plan 1;

my @a = 1, 2, 3;
my @b = <a b c>;
my \c = @a Z=> @b;
.so for c;
my $msg;
{ .so for c; CATCH { default { $msg = .Str } } }
is $msg,
    "The iterator of this Seq is already in use/consumed by another Seq (you\nmight solve this by adding .cache on usages of the Seq, or by assigning\nthe Seq into an array)",
    'X::Seq::Consumed message carries rakudo\'s line breaks';
