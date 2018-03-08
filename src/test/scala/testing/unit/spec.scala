package testing.unit

import java.io.File
import java.nio.file.{Files, Paths}

import org.scalatest.FlatSpec
import testing.{Client, TransAction, mainClass}

import scala.io.Source
import scala.util.Random

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
    val fileName = "files/testFiles/writeTest.txt"
    if (Files.exists(Paths.get(fileName))){
      new File(fileName).delete()
    }
    val users = List(new Client("", 1, 100, Array(1, 2, 3, 4)))
    mainClass.writeUsers(users,fileName)
    assert(Files.exists(Paths.get(fileName)))
  }

  it should "follow the format" in {
    val fileName = "files/testFiles/testReadWrite.txt"
    val fileName2 = "files/testFiles/testReadWrite2.txt"
    val users = mainClass.readClients(fileName)
    mainClass.writeUsers( users, fileName2)

    assert (Source.fromFile(fileName).getLines() sameElements Source.fromFile(fileName2).getLines())
  }

  behavior of "applyTransActions"

  it should "not change the entire number of $ and stocks" in {
    val fileName = "files/testFiles/testReadWrite.txt"
    val users = mainClass.readClients(fileName)

    val r = Random
    val transactions = List.fill(100)(new TransAction(r.nextInt(users.length),r.nextInt(users.length),r.nextInt(4),r.nextInt(100),r.nextInt(10)))

    val users2 = mainClass.applyTransActions(users,transactions)

    val collapsedData = users.foldLeft(new Client("",0,0,Array(0,0,0,0)))((x:Client,y:Client) =>
      new Client("",0,x.money+y.money,Array(x.stocks(0)+y.stocks(0),x.stocks(1)+y.stocks(1),x.stocks(2)+y.stocks(2),x.stocks(3)+y.stocks(3))))
    val collapsedData2 = users2.foldLeft(new Client("",0,0,Array(0,0,0,0)))((x:Client,y:Client) =>
      new Client("",0,x.money+y.money,Array(x.stocks(0)+y.stocks(0),x.stocks(1)+y.stocks(1),x.stocks(2)+y.stocks(2),x.stocks(3)+y.stocks(3))))

    assert(collapsedData.money == collapsedData2.money)
    assert(collapsedData.stocks sameElements  collapsedData2.stocks)
  }

  behavior of "ordersComparator"

  it should "be applicable for sorting" in {
    val fileName = "files/testFiles/correctOrders.txt"
    val orders = mainClass.readOrders(fileName)
    val orders2 = orders.sortWith(mainClass.ordersComparator)
    assert(orders.toSet == orders2.toSet)
  }

  //behavior of "main"
  //integral test
}
