import sys
import re
import datetime

REGEX = re.compile("- \[ \]")


class Task(object):
    def __init__(self, offset, value, date_predicate):
        self.offset = offset
        self.value = value
        self.date_predicate = date_predicate

    @classmethod
    def parse(cls, text, offset):
        """
        Assumes that the given offset really does point to a task.
        """
        value_offset = offset + 6 # skip the "- [ ] "

        date_predicate = None
        date_part = Task.try_parse_date(text, value_offset)

        if date_part is not None:
            date_predicate = date_part[0]
            value_offset += date_part[1]
            # skip the ": " after the date, if there is one
            if text[value_offset:value_offset+2] == ": ":
                value_offset += 2
            else:
                while text[value_offset] == " ":
                    value_offset += 1

        finish_offset = text.find("\n", value_offset) # find the end of the line
        if finish_offset == -1:
            finish_offset = len(text) # if there's no \n later in the text, just go to the end of the text
        value = text[value_offset:finish_offset]
        return Task(offset, value, date_predicate)

    @classmethod
    def find_all(cls, text):
        return [Task.parse(text, match.start()) for match in REGEX.finditer(text)]

    @classmethod
    def find_next(text, offset):
        pass

    @classmethod
    def find_previous(text, offset):
        pass

    @classmethod
    def try_parse_date(cls, text, offset):
        length = 0
        if text[offset] == ">":
            offset += 1
            length += 1
        if len(text) < offset+10:
            # text doesn't have room for a date
            return None
        if (text[offset:(offset+4)].isdigit() # year component
                and text[offset+4] == "-" 
                and text[offset+5:offset+7].isdigit() # month component
                and text[offset+7] == "-"
                and text[offset+8:offset+10].isdigit()):
            length += 10
            return (text[offset:(offset+10)], length)
        return None


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
