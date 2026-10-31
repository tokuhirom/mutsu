sub EXPORT(*@options) {
    %( |('True' => 'shadowed' unless @options), 'marker' => 5 )
}
