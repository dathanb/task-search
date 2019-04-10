import unittest
import task_search


class FindTaskTest(unittest.TestCase):
    def test_ignores_text_before_starting_offset(self):
        result = task_search.find_task(2, "- [ ] first\n- [ ] second")
        self.assertEqual(15, result, "Should ignore the leading line; expected 2, got {}".format(result))

    def test_when_no_next_task_returns_original_offset(self):
        result = task_search.find_task(2, "foo")
        self.assertEqual(2, result)


class FindTaskHelperTest(unittest.TestCase):
    def test_when_searching_from_the_start_of_a_task_it_skips_it(self):
        result = task_search.find_task_helper(0, 0, "- [ ] 1\n- [ ]")
        self.assertEqual(11, result)


if __name__ == "__main__":
    unittest.main()

