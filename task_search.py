import sys
import re

REGEX = re.compile("- \[ \]")

def find_task(offset, text):
    """ 
    Given a buffer, finds the next markdown-formatted checklist item (we'll call it a task) that hasn't yet been
    completed.

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
    """
    return find_task_helper(offset, offset, text)


def find_task_helper(starting_offset, search_offset, text):
    val = REGEX.search(text, search_offset)
    #print "starting_offset: {}, search_offset: {}, text: {}, val: {}".format(starting_offset, search_offset, text, val)
    if val is None:
        return starting_offset
    elif val.start() == search_offset: # search offset already points to an actual tag
        return find_task_helper(starting_offset, search_offset+1, text)
    return val.start() + 3 # increment by 3 to point to the place where the X should go


if __name__ == "__main__":
    byte_offset = sys.argv[1]
    text = sys.stdin.read()
    # print text
    print find_task(int(byte_offset), text)
