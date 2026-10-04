unit module WordListNamedImport;

our sub word_join(:@args) is export { @args.join(',') }
