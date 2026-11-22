use Test;

# `Str.subst` refuses `:ov` / `:ex` with X::Str::Subst::Adverb (#18897).

plan 6;

throws-like { "aaa".subst(/a/, "b", :ov) }, X::Str::Subst::Adverb,
    name => 'ov', message => 'Cannot use :ov adverb in Str.subst, got True',
    ':ov is refused';
throws-like { "aaa".subst(/a/, "b", :ex(1)) }, X::Str::Subst::Adverb,
    name => 'ex', message => 'Cannot use :ex adverb in Str.subst, got 1',
    ':ex is refused';
is "aaa".subst(/a/, "b", :ov(False)), 'baa', 'a false :ov is no adverb';
is "aaa".subst(/a/, "b", :ex(False)), 'baa', 'a false :ex is no adverb';
is "aaa".subst(/a/, "b", :g), 'bbb', ':g still works';
my $s = "aaa";
throws-like { $s.subst(/a/, "b", :ex) }, X::Str::Subst::Adverb, 'on a variable too';
