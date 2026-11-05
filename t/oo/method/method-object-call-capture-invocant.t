use Test;

# A Method object called with a variable spread through a capture keeps its
# invocant: the argument arrives as the variable's container, and the method's
# `self` is the value inside it.

plan 9;

class App {
    method who { self.defined ?? "inst" !! "type" }
    method two($x) { self.defined ?? "inst $x" !! "type $x" }
}
my $o = App.new;
my $who = $o.^find_method("who");
my $two = $o.^find_method("two");

is $who($o), 'inst', 'a direct call';

sub one(|c) { $who(|c) }
is one($o), 'inst', 'a one-element capture of a variable';
is one(App.new), 'inst', 'a one-element capture of a temporary';

sub withargs(|c) { $two(|c) }
is withargs($o, 5), 'inst 5', 'a capture with an argument after the invocant';

sub slurpy(*@p) { $who(|@p) }
is slurpy($o), 'inst', 'a slurpy array spread';

sub viaList(|c) { $who(|c.list) }
is viaList($o), 'inst', 'the capture\'s list spread';

my \cap = \($o);
is $who(|cap), 'inst', 'a stored capture';
is $who(|\($o)), 'inst', 'a capture literal';

is one(App), 'type', 'a type object invocant is still a type object';
