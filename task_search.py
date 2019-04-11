import sys
import re

REGEX = re.compile("- \[ \]")

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
