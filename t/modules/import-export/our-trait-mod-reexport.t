use lib 't/lib';
use Test;

plan 1;

use TodoTicket7989TraitB;

class TodoTicket7989Consumer {
    has $.value is marked;
}

ok TodoTicket7989Consumer.^attributes[0].todo_ticket_7989_marked,
    'a trait_mod re-exported through OUR:: handles an importer attribute';
