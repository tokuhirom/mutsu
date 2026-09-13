module TodoTicket7989TraitA {
    role Marked {
        method todo_ticket_7989_marked { True }
    }

    multi sub trait_mod:<is> (Attribute $attr, :$marked!) is export {
        $attr does Marked;
    }
}
