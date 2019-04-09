import unittest
import task_search

# TODO: remove skip_lines in favor of just getting the byte offset directly from vimp
class SkipLinesTest(unittest.TestCase):
    def test_skip_lines_with_0_skips_nothing(self):
        text = "asdfasdfafasdfasdfdsf"
        result = task_search.skip_lines(0, text)
        self.assertEqual(0, result)

    def test_skip_lines_can_skip_one_line(self):
        text = "asdf\njkl;"
        result = task_search.skip_lines(1, text)
        self.assertEqual(5, result)

    def test_skip_lines_can_skip_multiple_lines(self):
        text = "a\ns\nd\nf\ng\nh\nj\nk\nl\n"
        result = task_search.skip_lines(3, text)
        self.assertEqual(6, result)

    def test_skip_lines_with_more_lines_than_exist_returns_empty_string(self):
        text = ""
        result = task_search.skip_lines(1, text)
        self.assertEqual(0, result)


class FindTaskTest(unittest.TestCase):
    def test_ignores_text_before_starting_line(self):
        result = task_search.find_task(2, "- [ ] first\n- [ ] second")
        self.assertEqual(12, result, "Should ignore the leading line; expected 2, got {}".format(result))

    def test_when_no_next_task_returns_original_offset(self):
        result = task_search.find_task(2, "foo")
        self.assertEqual(2, result)


if __name__ == "__main__":
    unittest.main()
