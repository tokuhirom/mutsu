use v6;
use Test;

plan 16;

# A `module`/`package`/`class` block's `my @a` / `my %h` lives in the
# package's static store once the block has run, and the block's routines
# reach it only through that store. An in-place mutation made from one of
# those routines used to land in a copy under the name in the routine's own
# env, so every other reader kept the empty container (#10343, the shape of
# `System::Passwd::populate-users`).

module Users {
    my @users;
    my %by-name;
    my Bool $loaded = False;
    my sub populate() {
        @users.push('root');
        %by-name<root> = 0;
        $loaded = True;
    }
    our sub get-it { populate(); "{@users.elems} {%by-name.elems} $loaded" }
    our sub again  { "{@users.elems} {%by-name.elems}" }
}
is Users::get-it(), '1 1 True', 'a lexical sub of the block mutates the block\'s @ and %';
is Users::again(), '1 1', '... and another routine of the block sees it';

module Direct {
    my @u;
    our sub add($x) { @u.push($x); @u.elems }
    our sub count { @u.elems }
}
Direct::add(1);
is Direct::add(2), 2, 'a routine sees its own earlier push';
is Direct::count(), 2, 'a sibling routine sees both pushes';

# Every in-place mutator, element store and element update.
module Ops {
    my @u;
    my %h;
    our sub t1 { push @u, 1; @u.append(2, 3); @u.unshift(0) }
    our sub t2 { @u[1]++; %h<x>++; %h<y> += 5 }
    our sub t3 { %h<n><m> = 1; @u.pop }
    our sub t4 { %h<x>:delete; @u.splice(0, 1) }
    our sub t5 { @u = 7, 8; %h = q => 1 }
    our sub show { @u.raku ~ ' ' ~ %h.sort.raku }
}
Ops::t1;
is Ops::show(), '[0, 1, 2, 3] ().Seq', 'push, append and unshift';
Ops::t2;
is Ops::show(), '[0, 2, 2, 3] (:x(1), :y(5)).Seq', 'element increments and an element `+=`';
Ops::t3;
is Ops::show(), '[0, 2, 2] (:n(${:m(1)}), :x(1), :y(5)).Seq', 'a nested element store and pop';
Ops::t4;
is Ops::show(), '[2, 2] (:n(${:m(1)}), :y(5)).Seq', ':delete and splice';
Ops::t5;
is Ops::show(), '[7, 8] (:q(1),).Seq', 'a whole-container assignment';

# The aggregate passed as an argument is the aggregate, not a Scalar holding
# it: a slurpy or a list routine iterates its elements.
sub slurp(*@a) { @a.elems }
module Args {
    my @users = 1, 2, 3;
    my %h = a => 1;
    our sub first-two { first { $_ == 2 }, @users }
    our sub slurped { slurp(@users) }
    our sub grepped { (grep { $_ > 1 }, @users).elems }
    our sub hash-slurped { slurp(%h) }
}
is Args::first-two(), 2, '`first` iterates a package-block array';
is Args::slurped(), 3, 'a slurpy flattens it';
is Args::grepped(), 2, '`grep` iterates it';
is Args::hash-slurped(), 1, 'a slurpy flattens a package-block hash into its pairs';

# A class body's statics take the same road.
class Log {
    my @log;
    my %seen;
    my sub note($x) { @log.push($x); %seen{$x}++ }
    method add($x) { note($x); self }
    method dump { @log.raku ~ ' ' ~ %seen.sort.raku }
}
Log.add(1).add(2).add(1);
is Log.dump, '[1, 2, 1] ("1" => 2, "2" => 1).Seq', 'a class body\'s @ and % mutated from a lexical sub';

# Passing the aggregate to a routine that mutates it still mutates it.
sub push-nine(@a) { @a.push(9) }
sub store-z(%h) { %h<z> = 1 }
module Passed {
    my @u = 1;
    my %h;
    our sub run { push-nine(@u); store-z(%h); @u.raku ~ ' ' ~ %h.raku }
}
is Passed::run(), '[1, 9] {:z(1)}', 'a callee mutating the passed aggregate mutates the store';

# A `package` block, with a plain (non-`my`) sub.
package Pkg {
    my @q;
    sub inner { @q.push(9) }
    our sub go { inner(); @q.elems }
}
Pkg::go();
is Pkg::go(), 2, 'a package block behaves the same';
