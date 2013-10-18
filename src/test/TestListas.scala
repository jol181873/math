package test

/**
 * Created by IntelliJ IDEA.
 * User: jol
 * Date: 01-abr-2010
 * Time: 12:03:18
 * To change this template use File | Settings | File Templates.
 */

object TestListas {
  def main(a :Array[String]) :Unit = {
    var b =List(List(1,2,3,4),List(4,5,6,7));
    println(b);
    println(b.flatten);
  }
}