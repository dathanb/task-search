Task Search
===========

A vimscript and an accompanying Python script for easily iterating over
markdown-formatted checklist items in a document.

If the next task lacks a date annotation, TaskSearch will return the location of that task. e.g.,

    - [ ] This task will always match

If the next task has a date annotation, TaskSearch will only stop on that task (i.e., return the location of it) if
the date is in the past. e.g.,

    - [ ] >2019-04-01: This task will only match after April 1, 2019 local time

And completed checklist items will never match. e.g.,

    - [X] This task will not match
    - [-] This task will not match

(I don't think the latter format is standard markdown, but it's how I represent tasks that haven't been done, but
I've decided not to do.) 
