use Log::Timeline::Output;
use JSON::Fast;

#| Outputs the log into a file in the JSONLines format (that is, one line of
#| JSON per logged event).
class Log::Timeline::Output::JSONLines does Log::Timeline::Output {
    #| The file to log into.
    has IO::Path $.path is required;

    #| The handle to write to.
    has IO::Handle $!handle = open($!path, :w);

    #| Logs an event.
    method log-event($type, Int $parent-id, Instant $timestamp, %data --> Nil) {
        $!handle.say: to-json :!pretty, {
            :m($type.module), :c($type.category), :n($type.name), :k(0),
            :p($parent-id), :t($timestamp.Rat), :d(%data)
        }
    }

    #| Logs the start of a task.
    method log-start($type, Int $parent-id, Int $id, Instant $timestamp, %data --> Nil) {
        $!handle.say: to-json :!pretty, {
            :m($type.module), :c($type.category), :n($type.name), :k(1),
            :i($id), :p($parent-id), :t($timestamp.Rat), :d(%data)
        }
    }

    #| Logs the end of a task.
    method log-end($type, Int $id, Instant $timestamp --> Nil) {
        $!handle.say: to-json :!pretty, {
            :m($type.module), :c($type.category), :n($type.name), :k(2),
            :i($id), :t($timestamp.Rat)
        }
    }

    #| Close the output file handle, flushing any events.
    method close(--> Nil) {
        $!handle.close;
    }
}
