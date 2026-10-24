use v6;
use Test;

# A closure made in a method of a class declared inside a ROLE body belongs to
# that class, not to the role: it must see the class's own file-scoped subs
# (mutsu#14870, Pakku::Command::Config's `map( -> $o { to-json($o) } )`).
plan 4;

role R {
  my class Config { ... }
  method fly  { Config.new.plain }
  method fly2 { Config.new.mapped }
  method fly3 { Config.new.nested }
  my class Config {
    method plain  { helper(1) }
    method mapped { (1, 2).map( -> $o { helper($o) } ).join(',') }
    method nested { (1, 2).map( -> $o { (3,).map( -> $p { helper($o + $p) } ).Str } ).join(',') }
    sub helper ( \x ) { "H{x}" }
  }
}
my class T does R { }

is T.new.fly, 'H1', 'a direct call sees the class sub';
is T.new.fly2, 'H1,H2', 'a closure in the method sees it too';
is T.new.fly3, 'H4,H5', 'so does a closure nested in a closure';

role Plain { method who { (1,).map( -> $o { self.^name } ).join } }
class T2 does Plain { }
is T2.new.who, 'T2', 'a composed role method keeps working';
