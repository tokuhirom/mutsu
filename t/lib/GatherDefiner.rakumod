use GatherConstProvider;
sub gd-closure() is export { -> { GCK } }
sub gd-plain() is export { gather { take GCK } }
sub gd-eager() is export { my @a = gather { take GCK }; @a }
sub gd-lazy() is export { lazy gather { take GCK } }
sub gd-lexical() is export { my $x = GCK; lazy gather { take $x } }
sub gd-loop() is export { lazy gather for GCR -> $n { take $n } }
