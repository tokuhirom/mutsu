unit module UnitExport::Cfg is export;

grammar INI {
    token TOP { 'a' }
}

class INI::Actions {
    method TOP($/) { make 42 }
}

our sub parse(Str $s) {
    INI.parse($s, :actions(INI::Actions.new)).ast;
}

our sub type-name() { INI.^name }
