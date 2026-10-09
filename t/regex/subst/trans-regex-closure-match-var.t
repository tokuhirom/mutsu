use Test;

# From Text::Emoji: the closure of a `Regex => Callable` trans rule sees the
# match as `$/`.
plan 3;

my %l = "+1" => "A", "-1" => "B";
is ":+1: x".trans(/ ":" <[\w+-]>+ ":" / => { %l{$/.substr(1, *-1)} }), "A x", '$/ is the match';
is ":+1: x".trans(/ ":" <[\w+-]>+ ":" / => { ~$/ }), ":+1: x", '~$/ is the matched text';
"keep" ~~ /ee/;
":+1:".trans(/ ":" <[\w+-]>+ ":" / => { "Z" });
is ~$/, "ee", 'the caller $/ is restored';
