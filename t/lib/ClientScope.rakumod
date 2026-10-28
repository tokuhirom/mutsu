unit module ClientScope;

# CLIENT:: is the lexical scope of the nearest caller living in another
# package (Prompt::Expand's `:$index = CLIENT::<$*INDEX> // 0`).
sub client-index(:$index = CLIENT::<$*CLIENT-INDEX> // 0) is export { $index }
sub client-direct() is export { CLIENT::<$*CLIENT-INDEX> }
sub client-via-helper() is export { client-index() }
