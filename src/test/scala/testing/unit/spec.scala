package testing.unit

import org.scalatest.FlatSpec
import testing.{Client, mainClass}

class spec extends FlatSpec{

  behavior of "readClients"

  it should "make a collection of users" in {
    assert(mainClass.readClients("files/clients.txt").head.getClass == new Client("",0,0,Array(0)).getClass)
  }

  it should "check input line arguments number is 9" in {
    assert(mainClass.readClients("files/clients.txt").lengthCompare(9) == 0)
  }

  it should "check first argument name format" in {
    assert(mainClass.readClients("files/testFiles/clientsTestFirstArg.txt").head.name == "C15")
  }

  it should "check numeric formats of other arguments" in {
    intercept[java.lang.NumberFormatException]{
      mainClass.readClients("files/testFiles/clientsNumericError.txt")
    }
  }

  it should "check file exists" in {
    intercept[java.io.FileNotFoundException] {
      mainClass.readClients("files/testFiles/noFile.txt")
    }
  }

  behavior of "readOrders"

  it should "check input line arguments number is 4" in {
    assert(mainClass.readOrders("files/testFiles/testOrders.txt").lengthCompare(3) == 0)
    assert(mainClass.readOrders("files/testFiles/testOrders.txt").head.cost == 14)
    assert(mainClass.readOrders("files/testFiles/testOrders.txt")(2).count == 4)
  }

  it should "check first argument name format" in {
    assert(mainClass.readOrders("files/testFiles/testOrdersNameError.txt").lengthCompare(2) == 0)
    assert(mainClass.readOrders("files/testFiles/testOrdersNameError.txt").head.cost == 13)
    assert(mainClass.readOrders("files/testFiles/testOrdersNameError.txt")(1).count == 4)
  }

  it should "check second argument format" in {
    assert(mainClass.readOrders("files/testFiles/testOrdersNameError2.txt").lengthCompare(2) == 0)
    assert(mainClass.readOrders("files/testFiles/testOrdersNameError2.txt").head.cost == 13)
    assert(mainClass.readOrders("files/testFiles/testOrdersNameError2.txt")(1).count == 4)
  }

  it should "check third argument format" in {
    assert(mainClass.readOrders("files/testFiles/testOrdersNameError3.txt").lengthCompare(2) == 0)
    assert(mainClass.readOrders("files/testFiles/testOrdersNameError3.txt").head.cost == 13)
    assert(mainClass.readOrders("files/testFiles/testOrdersNameError3.txt")(1).count == 4)
  }

  it should "check numeric formats" in {
    intercept[java.lang.NumberFormatException]{
      mainClass.readOrders("files/testFiles/testOrdersNameError4.txt")
    }
  }

  it should "check file exists" in {
    intercept[java.io.FileNotFoundException] {
      mainClass.readOrders("files/testFiles/noFile.txt")
    }
  }

  behavior of "writeUsers"

  it should "create a file" in {

  }

  it should "follow the format" in {

  }

  behavior of "applyTransActions"

  it should "not change the entire number of $ and stocks" in {

  }

  behavior of "ordersComparator"

  it should "be applicable for sorting" in {

  }

  //behavior of "main"
  //integral test
}
