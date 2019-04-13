import sys
import re

REGEX = re.compile("- \[ \]")


class Task(object):
    def __init__(self, offset, value):
        self.offset = offset
        self.value = value

    @classmethod
    def parse(cls, text, offset):
        """
        Assumes that the given offset really does point to a task.
        """
        start_offset = offset + 6 # skip the "- [ ] "
        finish_offset = text.find("\n", offset) # find the end of the line
        if finish_offset == -1:
            finish_offset = len(text) # if there's no \n later in the text, just go to the end of the text
        value = text[start_offset:finish_offset]
        return Task(offset, value)

    @classmethod
    def find_all(cls, text):
        return [Task.parse(text, match.start()) for match in REGEX.finditer(text)]

    @classmethod
    def find_next(text, offset):
        pass

    @classmethod
    def find_previous(text, offset):
        pass


def find_next_task(offset, text):
    task_positions = [match.start() for match in REGEX.finditer(text) if match.start() > offset]
    if len(task_positions) > 0:
        return task_positions[0] + 3
    return offset


def find_previous_task(offset, text):
    task_positions = [match.start() for match in REGEX.finditer(text) if match.start() < offset - 3]
    if len(task_positions) > 0:
        return task_positions[-1] + 3
    return offset


def main(operation, offset, text):
    if operation == "next":
        return find_next_task(int(byte_offset), text)
    elif operation == "previous":
        return find_previous_task(int(byte_offset), text)


if __name__ == "__main__":
    direction = sys.argv[1]
    byte_offset = sys.argv[2]
    text = sys.stdin.read()
    val = main(direction, byte_offset, text)
    print val
