package Thesis.Thesisbuild
import stapl.core.pdp._

object TestTrees {

  import stapl.core._
  import complexityTest._
  
  def main(args: Array[String]) {
    var pdp = new PDP(comp1)
    pdp = new PDP(comp2)
    pdp = new PDP(comp3)
    pdp = new PDP(comp4)
    pdp = new PDP(comp5)
    println("Great success")
}
  
  def test1(pdp : PDP) = pdp.evaluate("test1", "view", "doc123")
  
  def test2(pdp : PDP) = pdp.evaluate("test2", "view", "doc123")
  
  def test3(pdp : PDP) = pdp.evaluate("test3", "view", "doc123")
  
  def test4(pdp : PDP) = pdp.evaluate("test4", "view", "doc123")
  
  def test5(pdp : PDP) = pdp.evaluate("test5", "view", "doc123")
  
  def test6(pdp : PDP) = pdp.evaluate("test6", "view", "doc123")
  
  def test7(pdp : PDP) = pdp.evaluate("test7", "view", "doc123")
  
  def test8(pdp : PDP) = pdp.evaluate("test8", "view", "doc123")
  
  def test9(pdp : PDP) = pdp.evaluate("test9", "view", "doc123")
  
  def test10(pdp : PDP) = pdp.evaluate("test10", "view", "doc123")
}