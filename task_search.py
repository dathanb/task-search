import sys
import re

REGEX = re.compile("- \[ \]")

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
def find_task(offset, text):
    val = REGEX.search(text, offset)
    if val is None:
        return offset
    return val.start()


def skip_lines(num_lines, text):
    """
    Returns the text offset of the beginning of the (num_lines + 1)th line.
    """
    if num_lines == 0:
        return 0
    text_index = 0
    line_count = 0
    while line_count < num_lines and text_index < len(text):
        if text[text_index] == "\n":
            line_count += 1
        text_index += 1
    return text_index


if __name__ == "__main__":
    byte_offset = sys.argv[1]
    text = sys.stdin.read()
    # print text
    print find_task(int(byte_offset), text)
