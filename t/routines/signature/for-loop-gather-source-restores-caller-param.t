use Test;

# #15208 (Selkie `Container.add` / `Widget.set-store`): a `for SOURCE -> $x`
# whose SOURCE is a `gather` must give the caller's same-named binding back
# when the loop is done. The lazy-gather loop path skipped the save/restore the
# eager path does, so the callee's last item replaced the calling method's `$x`.

plan 4;

class Leaf { }
class Tree {
    method kids { gather { take Leaf.new } }
    method walk() { for self.kids -> $child { } }
    method walk-eager() { for (Leaf.new,) -> $child { } }
}
class Holder {
    method add($child) { $child.walk; $child }
    method add-eager($child) { $child.walk-eager; $child }
}

is Holder.new.add(Tree.new).^name, 'Tree', 'the caller keeps its param after a gather-sourced for';
is Holder.new.add-eager(Tree.new).^name, 'Tree', 'and after an eager one';

sub outer($child) { inner(); $child }
sub inner() { for gather { take 5; take 6 } -> $child { } }
is outer('mine'), 'mine', 'a sub caller too';

my $x = 'outer-x';
sub loop-it() { for gather { take 1 } -> $x { } }
loop-it();
is $x, 'outer-x', 'a file-level lexical of the same name is untouched';
