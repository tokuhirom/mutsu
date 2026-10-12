unit module QualifiedEnumExport;

# Native::Packing (zef) declares `my enum Native::Packing::Endian is
# export(:Endian)`; the importer reaches the type by its last name part.
my enum QualifiedEnumExport::Endian is export(:Endian) <Network Vax Host>;
