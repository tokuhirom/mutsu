unit module NestedNamePk;

role Cl { }

class Window::Clauses does Cl {
    has @.items;
    multi method COERCE(Any $x) { self.new(:items[$x]) }
}

class Sel {
    has Window::Clauses $.windows;
}

our sub sel-plain(Window::Clauses $windows?) is export(:functions) {
    Sel.new(:$windows).windows.^name
}

our sub optional-name(Window::Clauses $w?) is export(:functions) { $w.^name }
