import sys
import re
import time

REGEX = re.compile("- \[ \]")


class Task(object):
    def __init__(self, offset, value, date_predicate):
        self.offset = offset
        self.value = value
        self.date_predicate = date_predicate

    def __eq__(self, other):
        if isinstance(other, Task):
            return self.offset == other.offset and self.value == other.value and self.date_predicate == other.date_predicate
        return False

    def __str__(self):
        return "{{offset: {0}, value: \"{1}\", date_predicate: {2}}}".format(self.offset, self.value, self.date_predicate)

    def __repr__(self):
        return "{{offset: {0}, value: \"{1}\", date_predicate: {2}}}".format(self.offset, self.value, self.date_predicate)

    def passes_date_predicate(self, date):
        return self.date_predicate is None or self.date_predicate <= date

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
    def find_next(cls, text, offset):
        tasks = Task.find_all(text)
        filtered_tasks = [task for task in tasks if task.offset > offset and task.passes_date_predicate(today())]
        if len(filtered_tasks) > 0:
            return filtered_tasks[0]
        return None

    @classmethod
    def find_previous(cls, text, offset):
        tasks = Task.find_all(text)
        filtered_tasks = [task for task in tasks if task.offset < offset - 3 and task.passes_date_predicate(today())]
        if len(filtered_tasks) > 0:
            return filtered_tasks[-1]
        return None

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


def today():
    return time.strftime("%Y-%m-%d", time.localtime())


def main(operation, offset, text):
    if operation == "next":
        task = Task.find_next(text, offset)
        if task == None:
            return offset
        return task.offset + 3 # position at the checkmark position
    elif operation == "previous":
        task = Task.find_previous(text, offset)
        if task == None:
            return offset
        return task.offset + 3 # position at the checkmark position


if __name__ == "__main__":
    direction = sys.argv[1]
    byte_offset = sys.argv[2]
    text = sys.stdin.read()
    val = main(direction, int(byte_offset), text)
    print val
