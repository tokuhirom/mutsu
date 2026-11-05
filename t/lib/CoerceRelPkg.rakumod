unit class CoerceRelPkg;
class Column::List { has @.e; multi method COERCE(Any $x) { self.new(e => [$x]) } }
class Sel { has Column::List $.c; }
our sub s(Column::List(Any) $c) { $c.^name }
our sub t(Column::List(Any) :$c) { Sel.new(:$c).c.^name }
our sub u(Column::List(Any) $c = 5) { $c.^name }
class Def {
    has Column::List $.pb;
    method COERCE(Map (Column::List(Any) :$pb)) { self.new(:$pb) }
}
our sub mk-def() { Def.COERCE(%()) }
