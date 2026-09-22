use Test;

# #9008: an INHERITED method whose own body declares a recursive nested
# `sub` (e.g. Tarjan's SCC algorithm, found via the Graph::Leaper ecosystem
# distribution) died "Unknown function: <name>" on its own self-recursive
# call, but only when invoked through a SUBCLASS that happens to declare its
# own, entirely unrelated class-body `sub`.
#
# Root cause: method dispatch anchors `current_package` to the class that
# has class-scoped subs so a method can find them by bare name (#8883) --
# but it keyed that decision on the dynamic RECEIVER's class rather than the
# class that lexically OWNS the method body. Calling an inherited method
# through a subclass with its own unrelated `sub` flipped `current_package`
# to the subclass for the whole call, and the nested sub's self-recursive
# call -- resolved under that now-mismatched package -- could no longer find
# itself.

plan 4;

class Base {
    method run() {
        my @result;
        sub strongconnect($v) {
            @result.push($v);
            strongconnect($v - 1) if $v > 0;
        }
        strongconnect(2);
        return @result;
    }
}

class Sub is Base {
    # An unrelated class-body sub is what used to flip current_package.
    sub check-moves($moves) { True }
}

is-deeply Base.new.run, [2, 1, 0],
    'a recursive nested sub in a method works when called on its own class';
is-deeply Sub.new.run, [2, 1, 0],
    'the same inherited method still recurses correctly through a subclass with its own class-body sub';

# The same shape, but through a role (the original ecosystem discovery site:
# Graph::Componentish's private !tarjan-scc composed into Graph, invoked via
# the Graph::Leaper subclass).
role Componentish {
    method !tarjan-scc(--> List) {
        my %seen;
        my @order;
        sub visit($v) {
            %seen{$v} = True;
            @order.push($v);
            for self.neighbors($v) -> $w {
                visit($w) unless %seen{$w}:exists;
            }
        }
        for self.vertex-list -> $v {
            visit($v) unless %seen{$v}:exists;
        }
        return @order.List;
    }
    method run { return self!tarjan-scc }
}

class Graph does Componentish {
    method vertex-list { <a b c> }
    method neighbors($v) {
        given $v { when 'a' { <b> }; when 'b' { <a c> }; default { () } }
    }
}

class GraphSub is Graph {
    sub helper($x) { $x }
}

is-deeply Graph.new.run.sort, <a b c>.sort,
    'a role-provided recursive private method works on the composing class';
is-deeply GraphSub.new.run.sort, <a b c>.sort,
    'the same role-provided recursive private method still works through a subclass with its own class-body sub';
