use Test;

plan 6;

role P[::T] {
    method type-only(::?CLASS:U:) { T.^name }
    method instance-only(::?CLASS:D:) { T.^name }
    method is-type() { !self.defined }
}

is P[Int].type-only, 'Int', 'a parameterized role type object binds a :U invocant';
is P[Str].type-only, 'Str', 'the role parameter is bound for another type';
ok P[Int].is-type, 'an ordinary role method receives the type object';
throws-like { P[Int].instance-only }, X::Parameter::InvalidConcreteness,
    'a :D invocant rejects the role type object';
is P[Int].new.instance-only, 'Int', 'a :D invocant accepts a punned instance';

role BinaryTree[::Type] {
    has BinaryTree[Type] $.left;
    has BinaryTree[Type] $.right;
    has Type $.node;

    method visit-preorder(&cb) {
        cb $.node;
        for $.left, $.right -> $branch {
            $branch.visit-preorder(&cb) if defined $branch;
        }
    }

    method new-from-list(::?CLASS:U: *@el) {
        my $middle-index = @el.elems div 2;
        my @left = @el[0 .. $middle-index - 1];
        my $middle = @el[$middle-index];
        my @right = @el[$middle-index + 1 .. *];
        self.new(
            node => $middle,
            left => @left ?? self.new-from-list(@left) !! self,
            right => @right ?? self.new-from-list(@right) !! self,
        );
    }
}

my @visited;
BinaryTree[Int].new-from-list(4, 5, 6).visit-preorder(-> $n { @visited.push($n) });
is @visited.join(','), '5,4,6', 'the documented BinaryTree type-object constructor runs';
