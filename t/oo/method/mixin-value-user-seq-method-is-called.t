use v6;
use Test;

plan 6;

role Tag { method of { "tagged" } }

class Plain { method Seq { "class-Seq" } }
role Seqish { method Seq { "role-Seq" } }
class FromRole does Seqish { }

is (Plain.new but Tag).Seq, "class-Seq", ".Seq of a class's mixin value reaches the class's method";
is (FromRole.new but Tag).Seq, "role-Seq", ".Seq of a mixin over a role-provided method reaches it";
is (FromRole.new but Tag).clone.Seq, "role-Seq", "also after .clone";

is ("a" but Tag).Seq.raku, '("a",).Seq', "a mixin over a Str is still a one-element Seq";
is (5 but Tag).Seq.raku, '(5,).Seq', "a mixin over an Int is still a one-element Seq";
is (Plain.new but Tag).of, "tagged", "the mixed-in method itself still works";
