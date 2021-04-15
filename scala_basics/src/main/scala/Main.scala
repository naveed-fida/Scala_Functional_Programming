import MyFunctions.factorial
import MyFunctions.fib
import MyFunctions.findFirst
import MyFunctions.isSorted

object Main {
  def abs(n: Int): Int =
    if (n < 0) -n else n

  private def formatAbs(n: Int): String =
    "The absolute value of %d is $d".format(n, abs(n))

  private def formatResult(name: String, n: Int, fn: Int => Int) = {
    val msg = "The %s of %d is: %d\n";
    printf(msg, name, n, fn(n));
  }

  private def intComparator(a: Int, b: Int): Boolean =
    a < b

  def main(args: Array[String]): Unit = {
    formatResult("absolute value", -5, abs);
    formatResult("factorial", 5, factorial);

    val strings = Array("a", "ab", "abc", "abcd", "abcdef");
    val idx = findFirst(strings, (str: String) => str.length > 3)
    printf("First string with length > 3: %s\n", strings(idx));

    printf("Sorted: %b\n", isSorted(Array(1, 2, 3, 0), (a: Int, b: Int) => a < b))
  }
}
