package testing

import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets

import scala.collection.mutable.ListBuffer
import scala.io.Source

class Client (val name:String, val ID:Int, var money:Int, var stocks:Array[Int]){
  override def toString: String = {
    name + '\t' + money + stocks.foldLeft("")(_ + '\t' + _) + '\n'
  }
}

//class for read data
class Order (val clientID:Int, val typeID:Int, val stockID:Int, val cost:Int, var count:Int)

class TransAction(val clientSellerID:Int, val clientBuyerID:Int, val stockID:Int, val cost:Int, val count:Int )

object mainClass {

  def readClients(file:String):List[Client] = {
    val lines = Source.fromFile(file).getLines()

    //extracting only correct lines
    val (correctLines,incorrectLines) = {
      lines map(x => x split '\t') partition (x =>
          x.length == 6 && //6 elements: name, $,A,B,C,D
          x(0).startsWith("C") && //let's imagine client name starts with "C" (could be replaced for any name-checker)
          !x.slice(1, 5).exists(y => Option(y.toInt).getOrElse(-1) < 0) //there is no negative or non-convertible initial values
      )
    }

    //printing incorrect lines
    for (incorrectLine <- incorrectLines){
      print ("user line has incorrect format and was ignored:\n\t" )
      for (elem:String <- incorrectLine) print(elem + '\t')
    }

    correctLines.map(x => {
      new Client(x(0), x(0).slice(1,x.length).toInt, x(1).toInt, x.slice(2,x.length).map(y => y.toInt))
    }).toList
  }

  def readOrders(file:String):List[Order] = {
    val lines = Source.fromFile(file).getLines()

    //extracting only correct lines
    val (correctLines,incorrectLines) = {
      lines map(x => x split '\t') partition (x =>
          x.length == 5 && //5 elements: name, type(s b), stock(ABCD), cost, count
          x(0).startsWith("C") && //let's imagine client name starts with "C" (could be replaced for any name-checker)
          (x(1)=="b" || x(1)=="s") && //two options: buy or sell
          x(2).length==1 && //only one character for stock name
          (x(2)(0)>='A' && x(2)(0)<='D') && //Stocks are marked from A to D
          !x.slice(3, 4).exists(y => Option(y.toInt).getOrElse(-1) < 0) //there is no negative or non-convertible initial values
        )
    }

    //printing incorrect lines
    for (incorrectLine <- incorrectLines){
      print ("order line has incorrect format and was ignored:\n\t" )
      for (elem:String <- incorrectLine) print(elem + '\t')
    }

    //matching maps for clients, order types and stocks
    def clientIDMap = Map("C1" -> 0, "C2" -> 1, "C3" -> 2, "C4" -> 3, "C5" -> 4, "C6" -> 5, "C7" -> 6, "C8" -> 7, "C9" -> 8 )
    def orderTypeMap = Map("b" -> 0, "s" -> 1)
    def stockMap = Map("A" -> 0, "B" -> 1, "C" -> 2, "D" -> 3)

    correctLines.map(x => {
      //(clientID:Int, typeID:Int, stockID:Int, cost:Int, count:Int)
      new Order(clientIDMap.getOrElse(x(0), -1),
        orderTypeMap.getOrElse(x(1), -1),
        stockMap.getOrElse(x(2), -1),
        x(3).toInt,
        x(4).toInt)
    }).toList
  }

  def writeUsers (users:List[Client],resultFile:String): Unit = {
    val resultString:String = users.map(x => x.toString).foldLeft("")(_+_)
    Files.write(Paths.get(resultFile), resultString.getBytes(StandardCharsets.US_ASCII))
  }

  // function gets initial array of clients and transactions list and apply transactions to clients scores
  def applyTransActions(clients: List[Client], transAction: List[TransAction]): List[Client] = {
    val newClients = clients
    transAction.foreach(ta => {
      //transfer money
      newClients(ta.clientBuyerID).money = newClients(ta.clientBuyerID).money - ta.cost*ta.count
      newClients(ta.clientSellerID).money = newClients(ta.clientSellerID).money + ta.cost*ta.count

      //transfer stocks
      newClients(ta.clientBuyerID).stocks(ta.stockID) = newClients(ta.clientBuyerID).stocks(ta.stockID) + ta.count
      newClients(ta.clientSellerID).stocks(ta.stockID) = newClients(ta.clientSellerID).stocks(ta.stockID) - ta.count
    })
    newClients
  }

  def ordersComparator (a:Order, b:Order):Boolean = {
    if (a.stockID == b.stockID){
      if (a.typeID == b.typeID){
        if (a.cost == b.cost){
          a.count < b.count
        }else {a.cost > b.cost}
      }else {a.typeID > b.typeID}
    }else {a.stockID > b.stockID}
  }

  def main(args: Array[String]):Unit = {
    //files location
    // could be replaced to "args"
    val initialDataFile = "files/clients.txt"
    val ordersFile = "files/orders.txt"
    val resultFile = "files/result.txt"

    //initial state of clients
    val clients = readClients(initialDataFile)
    //list of orders
    val orders:List[Order] = readOrders(ordersFile)

    //mutable state of the current orders
    val state:ListBuffer[Order] = ListBuffer()
    var transAction:ListBuffer[TransAction] = ListBuffer()
    //(val clientID:Int, val typeID:Int, val stockID:Int, val cost:Int, val count:Int)

    //Comparator with priority: stockID, typeID, cost, -count


    //checking all orders and much them with respect to the order
    orders.foreach( order => {
      //computing index of matched previous order
      //same stock, different type of operation (buy-sell), different clients (no meta-marketing), matched cost conditions
      var i = if (order.typeID ==0){
        //new order is for buy
        state.lastIndexWhere(z => z.stockID == order.stockID && (z.clientID != order.clientID) && z.typeID != order.typeID && z.cost<=order.cost)
      } else {
        //new order is for sell
        state.indexWhere(z => z.stockID == order.stockID && (z.clientID != order.clientID) && z.typeID != order.typeID && z.cost>=order.cost)
      }

      //if no matching found - add order as active for the state collection
      if (i == -1){
        state += order
      }else{
        //else make transaction(s)
        //transactions could be stored and applied because of negative balance capability
        val tempOrder:Order = order
        while (tempOrder.count >0 && i > -1){
          if (order.typeID == 0){
            //buy branch
            if (state(i).count > order.count){
              //old order is "stronger"
              state(i).count = state(i).count.-(tempOrder.count)
              transAction :+ new TransAction(tempOrder.clientID,state(i).clientID,tempOrder.stockID,tempOrder.cost,tempOrder.count)
              tempOrder.count = 0
            }else{
              //new order is "stronger"
              tempOrder.count = tempOrder.count - state(i).count
              transAction :+ new TransAction(tempOrder.clientID,state(i).clientID,tempOrder.stockID,tempOrder.cost,state(i).count)
            }
          }else{
            //sell branch
            if (state(i).count > order.count){
              //old order is "stronger"
              state(i).count = state(i).count.-(tempOrder.count)
              transAction :+ new TransAction(state(i).clientID,tempOrder.clientID,tempOrder.stockID,state(i).cost,tempOrder.count)
              tempOrder.count = 0
            }else{
              //new order is "stronger"
              tempOrder.count = tempOrder.count - state(i).count
              //state(i).count = 0
              transAction += new TransAction(tempOrder.clientID,state(i).clientID,tempOrder.stockID,state(i).cost,state(i).count)
              state.remove(i)
            }
          }
          //looking for another matched existing order
          i = if (order.typeID ==0){
            state.lastIndexWhere(z => z.stockID == order.stockID && (z.clientID != order.clientID) && z.typeID != order.typeID && z.cost<=order.cost)
          } else {
            state.indexWhere(z => z.stockID == order.stockID && (z.clientID != order.clientID) && z.typeID != order.typeID && z.cost>=order.cost)
          }
        }
        // if new order is not done and complete, semi-done version should be added to the state
        if  (i == -1 && tempOrder.count > 0){
          state += tempOrder
        }
      }
    })

    //applying transactions to the clients data and writing the file
    writeUsers(applyTransActions(clients,transAction.toList),resultFile)
  }
}
