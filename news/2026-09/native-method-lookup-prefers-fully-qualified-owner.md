Native method lookup now prefers an exact fully-qualified owner before trying a
short class name. Generated accessors therefore remain callable when another
class registers a native method with the same basename.
