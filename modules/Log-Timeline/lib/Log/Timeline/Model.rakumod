use Log::Timeline::Output;

#| Role done by various representations of an ongoing task.
role Log::Timeline::Ongoing {
    #| The ID of the ongoing task, or zero if there is none.
    method id() { ... }

    #| Called to end the task.
    method end(--> Nil) { ... }
}

#| Object tracking an ongoing task.
class Log::Timeline::Ongoing::Logged does Log::Timeline::Ongoing {
    #| Tasks are numbered from 1, so we can use zero to indicate the absence
    #| of a task.
    my atomicint $current-id = 1;

    #| The ongoing task.
    has $.task is required;

    #| The task's unique ID.
    has int $.id = $current-id⚛++;

    #| The output to log the end to.
    has Log::Timeline::Output $.output is required;

    method end(--> Nil) {
        $!output.log-end($!task, $!id, now);
    }
}

#| When we are not logging, we do not do anything at the task end point.
#| This object simply does nothing when `end` is called on it.
class Log::Timeline::Ongoing::Unlogged does Log::Timeline::Ongoing {
    method id() { 0 }
    method end(--> Nil) {}
}

#| A single event that occurs at a point in time.
role Log::Timeline::Event[Str $module, Str $category, Str $name] {
    #| The module name of the event/task.
    method module(--> Str) { $module }

    #| The category of the event/task.
    method category(--> Str) { $category }

    #| The name of the event/task.
    method name(--> Str) { $name }

    #| Log an event of this type. The named parameters passed provide
    #| extra data about the event. The parent task will be taken from
    #| C<$*LOG-TIMELINE-CURRENT-TASK>, if there is one.
    multi method log(*%data --> Nil) {
        with PROCESS::<$LOG-TIMELINE-OUTPUT> {
            self!log-internal($_, $*LOG-TIMELINE-CURRENT-TASK // Nil, %data)
        }
    }

    #| Log an event of this type. The parent parameter is used to manually
    #| set the parent task; if there should be no parent, an undefined object
    #| (such as Nil) should be passed. The named parameters provide extra data
    #| about the event.
    multi method log($parent, *%data --> Nil) {
        with PROCESS::<$LOG-TIMELINE-OUTPUT> {
            self!log-internal($_, $parent, %data)
        }
    }

    method !log-internal(Log::Timeline::Output $output, $parent, %data --> Nil) {
        $output.log-event(self, $parent.?id // 0, now, %data)
    }
}

#| An task with a start and end time.
role Log::Timeline::Task[Str $module, Str $category, Str $name] {
    #| The module name of the event/task.
    method module(--> Str) { $module }

    #| The category of the event/task.
    method category(--> Str) { $category }

    #| The name of the event/task.
    method name(--> Str) { $name }

    #| Log a task starting and ending at specified instants.
    method log-fixed(Instant $from, Instant $to, *%data, :$parent) {
        with PROCESS::<$LOG-TIMELINE-OUTPUT> {
            my $ongoing = Log::Timeline::Ongoing::Logged.new(:task(self), output => $_); # output=> could pass Any
            .log-start($ongoing.task, $parent.?id // 0, $ongoing.id, $from, %data);
            .log-end($ongoing.task, $ongoing.id, $to);
        }
    }

    #| Runs a task, logging its start and end time along with the specified
    #| data. The parent task will be taken from C<$*LOG-TIMELINE-CURRENT-TASK>,
    #| if there is one.
    multi method log(&task, *%data) {
        with PROCESS::<$LOG-TIMELINE-OUTPUT> {
            my $ongoing = self!start-internal($_, $*LOG-TIMELINE-CURRENT-TASK // Nil, %data);
            LEAVE $ongoing.end();
            do {
                my $*LOG-TIMELINE-CURRENT-TASK := $ongoing;
                &task.count == 0 ?? task() !! task($ongoing)
            }
        }
        else {
            &task.count == 0 ?? task() !! task(Log::Timeline::Ongoing::Unlogged)
        }
    }

    #| Runs a task, logging its start and end time along with the specified
    #| data. It will be logged as a sub-task of the specified parent; pass an
    #| undefined object such as C<Nil> to make it have no parent task.
    multi method log($parent, &task, *%data) {
        with PROCESS::<$LOG-TIMELINE-OUTPUT> {
            my $ongoing = self!start-internal($_, $parent, %data);
            LEAVE $ongoing.end();
            do {
                my $*LOG-TIMELINE-CURRENT-TASK := $ongoing;
                &task.count == 0 ?? task() !! task($ongoing)
            }
        }
        else {
            &task.count == 0 ?? task() !! task(Log::Timeline::Ongoing::Unlogged)
        }
    }

    #| Logs the start of a task. The parent task will be taken from
    #| C<$*LOG-TIMELINE-CURRENT-TASK>, if there is one. Call end on the returned
    #| object to log the end of the task. Prefer to use the C<task> method where
    #| possible; calling start/end manually is intended for situations where the
    #| start and end points are spread over different lexical scopes.
    multi method start(*%data --> Log::Timeline::Ongoing) {
        with PROCESS::<$LOG-TIMELINE-OUTPUT> {
            self!start-internal($_, $*LOG-TIMELINE-CURRENT-TASK // Nil, %data);
        }
        else {
            Log::Timeline::Ongoing::Unlogged
        }
    }

    #| Logs the start of a task, being a sub-task of the specified parent task.
    #| Pass Nil if this task should not be considered a child of any other task.
    #| Returns an ongoing task object. Call end on it to log the end of the task.
    #| Prefer to use the C<log> method where possible; calling start/end
    #| manually is intended for situations where the start and end points are
    #| spread over different lexical scopes.
    multi method start($parent, *%data --> Log::Timeline::Ongoing) {
        with PROCESS::<$LOG-TIMELINE-OUTPUT> {
            self!start-internal($_, $parent, %data);
        }
        else {
            Log::Timeline::Ongoing::Unlogged
        }
    }

    method !start-internal(Log::Timeline::Output $output, $parent, %data) {
        my $ongoing = Log::Timeline::Ongoing::Logged.new(:task(self), :$output);
        $output.log-start(self, $parent.?id // 0, $ongoing.id, now, %data);
        $ongoing
    }
}
