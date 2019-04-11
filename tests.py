import unittest
import task_search


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

