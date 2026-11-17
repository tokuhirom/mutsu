unit module BwRel::Mod;

# `Cell` read from this module's routines names `BwRel::Mod::Cell`; read from a
# method of `Grid`, which declares its own `Cell`, it names `BwRel::Mod::Grid::Cell`.
class Cell { method kind { 'module cell' } }

class Grid {
    class Cell { method kind { 'grid cell' } }
    method make-cell { make-cell() }
    method own-cell { Cell.new }
}

role Maker {
    method role-cell { Cell.new }
}

class Sheet does Maker { }

our sub make-cell { Cell.new }

our sub make-many($n) { (^$n).map({ Cell.new.kind }).List }
