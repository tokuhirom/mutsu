use Test;

plan 1;

# A role method is dispatched through a class pun. The pun must retain the
# default type capture even when a typed named parameter follows it.
role Renderer { }
class DefaultRenderer does Renderer { }
role TreeLike[::ValueType = Any, Renderer :$renderer = DefaultRenderer] {
    has ValueType $.value;
    method make-value {
        self.new(value => "text").value.^name;
    }
}

is TreeLike.make-value, 'Str',
    'a role-punned method sees a defaulted type capture before a named parameter';
