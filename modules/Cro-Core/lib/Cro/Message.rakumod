# A Cro::Message is, in the abstract, some kind of message that a Cro
# application is processing. It might be a message from a message queue, a
# ZeroMQ message, a HTTP request, a HTTP response, etc.
role Cro::Message {
    # cw: For OOB attributes associated with the messages. Consider
    #     that there is no way to know who the message is from,
    #     or where it is supposed to go.
    has %.attributes;


    # Provides trace output for use with CRO_TRACE=1 and  `cro trace ...`.
    # Should include a developer-friendly view of the message. The default is
    # just to show the message type
    method trace-output(--> Str) {
        self.^name
    }

    # Utility method for providing trace output of a blob, as a hex dump.
    method !trace-blob(Blob $b) {
        my $limit = %*ENV<CRO_TRACE_MAX_BINARY_DUMP> // 512;
        my @pieces;
        loop (my int $i = 0; $i <= ($limit min $b.elems); $i += 16) {
            my @line := $b[$i .. ($i + (16 min ($b.elems - $i))) - 1];
            my $hex-dump = @line.fmt('%02x', ' ');
            my $padding = ' ' x (1 + 16 * 3 - $hex-dump.chars);
            my $decode = @line.map({ 32 <= $_ <= 126 ?? chr($_) !! '.' }).join;
            push @pieces, $hex-dump, $padding, $decode, "\n";
        }
        if $limit < $b.elems {
            push @pieces, "[{$b.elems - $limit} bytes not displayed]\n";
        }
        @pieces.join
    }
}
