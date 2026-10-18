unit module UseInBlockQualifiedPkg;
class Foo is export { method hi { 'foo' } }
our sub osub { 'osub' }
our sub own { ::('UseInBlockQualifiedPkg::Foo').hi }
