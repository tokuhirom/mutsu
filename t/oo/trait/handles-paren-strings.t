use Test;

# `handles(...)` with a *parenthesized* list of quoted method names (and bare
# identifiers), optionally mixed with `exposed => 'target'` rename pairs, is
# valid Rakudo syntax. mutsu only accepted the bare `handles <a b>` / `handles
# "a"` forms; the parenthesized-string form failed to parse, so `handles(...)`
# was misread as a call and the attribute reference lost its `self`.

plan 9;

class Book {
    has Str  $.title;
    has Str  $.author;
    has Str  $.language;
    has Cool $.publication;
}

class Product {
    has Book $.book handles('title', 'author', 'language', year => 'publication');
}

my $book = Book.new(:title<Dune>, :author('Frank Herbert'),
                    :language<English>, :publication<1965>);
my $p = Product.new(:book($book));
is $p.title,    'Dune',          'quoted string method delegates';
is $p.author,   'Frank Herbert', 'second quoted string method delegates';
is $p.language, 'English',       'third quoted string method delegates';
is $p.year,     '1965',          'mixed fat-arrow rename delegates';

# Bare identifiers in the list
class P2 { has Book $.book handles(title, author); }
my $p2 = P2.new(:book($book));
is $p2.title,  'Dune',          'bare identifier method delegates';
is $p2.author, 'Frank Herbert', 'second bare identifier delegates';

# Private attribute with mixed rename + bare-string delegation (the Queue idiom)
class Queue {
    has @!q handles(enqueue => 'push', dequeue => 'shift', 'elems');
    method contents { @!q.join(',') }
}
my $q = Queue.new;
$q.enqueue($_) for 1..3;
is $q.elems, 3, 'bare-string method on a private @-attribute';
is $q.dequeue, 1, 'rename on a private @-attribute';
is $q.contents, '2,3', 'remaining elements after dequeue';
