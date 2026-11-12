# A module whose own code applies the imported `eq`, as Red's SQLiteMaster
# does in `method is-table { self.type eq "table" }`.
use OpAst;
class OpModel {
    method column { OpAst.new(:name<type>) }
    method is-table { self.column eq 'table' }
}
