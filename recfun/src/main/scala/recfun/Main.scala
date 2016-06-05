package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    *
    * pascal(x, y) = sum( pascal(x-1, y-1)). se x == -1 || y == -1 then 0
    *
    * @param c
    * @param r
    * @return
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * This method computes the sum of all elements in the list xs. There are
    * multiple techniques that can be used for implementing this method, and
    * you will learn during the class.
    *
    * For this example assignment you can use the following methods in class
    * `List`:
    *
    *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
    *  - `xs.head: Int` returns the head element of the list `xs`. If the list
    *    is empty an exception is thrown
    *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
    *    list `xs` without its `head` element
    *
    *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
    *  solution.
    *
    * @param xs A list of natural numbers
    * @return The sum of all elements in `xs`
    */
  def sum(xs: List[Int]): Int = if (xs.isEmpty) 0 else xs.head + sum(xs.tail)

  def ++(n : Int) : Int = n + 1;

  def balance(chars: List[Char]): Boolean = {
    def countingPar(chars: List[Char], mapPar : collection.mutable.Map[String, Int]) : collection.mutable.Map[String, Int] ={
      if (chars.isEmpty) mapPar;
      else if ( chars.head == ')') {
        var number = mapPar.get(")") match {
          case Some(i) => i;
          case None => 0;
        }
        number = ++(number);
        mapPar.put(")",number);
        countingPar(chars.tail, mapPar);
      }

      else if ( chars.head == '(') {
        var number = mapPar.get("(") match {
          case Some(i) => i;
          case None => 0;
        }
        number = ++(number);
        mapPar.put("(",number);
        countingPar(chars.tail, mapPar);
      }
      else {
        countingPar(chars.tail, mapPar);
      }
    }
    val result = countingPar(chars, collection.mutable.Map(")" -> 0, "(" -> 0));
    val open = result.get("(");
    val close = result.get(")");
    open == close;
  }

  /*def countChange(money: Int, coins: List[Int]): Int = {
    def sumAccumulator(coins: List[Int], sum: Int): Int = {
      if (sum == money) {
        1
      } else if (sum > money || coins.isEmpty) {
        0
      } else {
        val newSum = sum + coins.head ;
        sumAccumulator(coins, newSum) + sumAccumulator(coins.tail, 0)
      }
    }
    return sumAccumulator(coins, 0);
  }*/

  def countChange(money: Int, coins: List[Int]): Int = {
    def count(sum: Int, c: List[Int]) : Int = {
      if (c.isEmpty) { 0}
      else if (sum - c.head < 0) {  0}
      else if (sum - c.head == 0){  1}
      else countChange(sum - c.head, c) + countChange(sum, c.tail)
    }
    count(money, coins.sorted)
  }
  /**
    * This method returns the largest element in a list of integers. If the
    * list `xs` is empty it throws a `java.util.NoSuchElementException`.
    *
    * You can use the same methods of the class `List` as mentioned above.
    *
    * ''Hint:'' Again, think of a recursive solution instead of using looping
    * constructs. You might need to define an auxiliary method.
    *
    * @param xs A list of natural numbers
    * @return The largest element in `xs`
    * @throws java.util.NoSuchElementException if `xs` is an empty list
    */
  def max(xs: List[Int]): Int = {
    def maxAccumulator(xs: List[Int], theMax: Int): Int = {
      if (xs.isEmpty) { theMax
      } else {
        val newMax : Int = if ( xs.head > theMax ) xs.head else theMax;
        maxAccumulator(xs.tail, newMax);
      }
    }
    if ( xs.isEmpty){
      throw new java.util.NoSuchElementException;
    }else{
      maxAccumulator(xs, 0);
    }
  }


}
