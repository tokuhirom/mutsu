use Test;
use lib 't/lib';

# PDF::Font::Loader declares this shape for its load-font method: the tagged
# proto method must export a callable sub along with its method candidates.
plan 2;

{
    use ExportedProtoMethodFixture :load-it;
    is load-it(:a(7)), 'a:7', 'the named tag imports a callable proto method';
}

{
    use ExportedProtoMethodFixture;
    nok defined(::('&load-it')), 'a tag-only proto method is absent from default exports';
}
