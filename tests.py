#!/usr/bin/env python

import datetime
import unittest
import task_search
from task_search import Task


class TaskParseTest(unittest.TestCase):
    def test_when_text_is_a_single_line_uses_to_the_end_of_the_line(self):
        text = "- [ ] this is the value"
        task = Task.parse(text, 0)
        self.assertEqual(task.offset, 0)
        self.assertEqual(task.value, "this is the value")

    def test_when_text_contains_multiple_newlines_stops_at_the_first(self):
        text = "- [ ] first\n- [ ] second"
        task = Task.parse(text, 0)
        self.assertEqual(task.offset, 0)
        self.assertEqual(task.value, "first")

    def test_parses_greater_than_date_predicate(self):
        text = "- [ ] >2019-05-01: foo"
        task = Task.parse(text, 0)
        self.assertEqual(task.offset, 0)
        self.assertEqual(task.value, "foo")
        self.assertEqual(task.date_predicate, '2019-05-01')


class FindAllTest(unittest.TestCase):
    def test_finds_all_tasks(self):
        text = "- [ ] first\n  - [ ] second\n- [ ] third"
        tasks = Task.find_all(text)
        self.assertEqual(tasks[0].offset, 0)
        self.assertEqual(tasks[0].value, "first")
        self.assertEqual(tasks[1].offset, 14)
        self.assertEqual(tasks[1].value, "second")
        self.assertEqual(tasks[2].offset, 27)
        self.assertEqual(tasks[2].value, "third")

    def test_when_no_tasks_returns_empty_list(self):
        text = "foo"
        tasks = Task.find_all(text)
        self.assertEqual(tasks, [])


class TryParseDateTest(unittest.TestCase):
    def test_without_enough_characters_to_fit_a_date_returns_none(self):
        text = "foo"
        result = Task.try_parse_date(text, 0)
        self.assertEqual(result, None)

    def test_with_a_valid_date_returns_the_date(self):
        text = "- [ ] 2019-01-01:"
        result = Task.try_parse_date(text, 6)
        self.assertEqual(result, ("2019-01-01", 10))

    def test_with_a_greater_than_date_returns_the_date(self):
        text = "- [ ] >2019-01-01: foo"
        result = Task.try_parse_date(text, 6)
        self.assertEqual(result, ("2019-01-01", 11))


class FindNextTaskTest(unittest.TestCase):
    def test_ignores_text_before_starting_offset(self):
        result = task_search.find_next_task(2, "- [ ] first\n- [ ] second")
        self.assertEqual(15, result, "Should ignore the leading line; expected 2, got {}".format(result))

    def test_when_no_next_task_returns_original_offset(self):
        result = task_search.find_next_task(2, "foo")
        self.assertEqual(2, result)


class FindPreviousTaskTest(unittest.TestCase):
    def test_when_offset_is_in_a_task_ignores_the_task(self):
        result = task_search.find_previous_task(13, "- [ ] first\n- [ ] second")
        self.assertEqual(3, result)

    def test_when_no_previous_task_returns_the_original_offset(self):
        result = task_search.find_previous_task(3, "- [ ] first\n- [ ] second")
        self.assertEqual(3, result)


if __name__ == "__main__":
    unittest.main()

