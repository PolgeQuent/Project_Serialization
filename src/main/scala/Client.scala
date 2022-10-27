import scala.util.{Failure, Success, Try}
import java.net._
import java.io._

import java.io.BufferedReader
import java.io.PrintWriter
import java.net.ServerSocket
import java.net.Socket

class ClientStub(clientSocket: Socket, out: PrintWriter, in: BufferedReader) extends OperationService:
  def sendRequest(request: String, id: String):
  Option[Int] = {
    out.println(request)
    val resp = in.readLine()
    val response = OperationResponse.serde.deserialize(resp)
    if(response.get._1.id == id) response.get._1.result else Option.empty
  }

  override def multiply(a: Int, b: Int): Option[Int] = {
    val req = OperationRequest(operator = Operator.MULTIPLY, a = a, b = b)
    val serialized = OperationRequest.serde.serialize(req)
    sendRequest(serialized, req.id)
  }


  override def add(a: Int, b: Int): Option[Int] = {
    val req = OperationRequest(operator=Operator.ADD, a=a, b=b)
    val serialized = OperationRequest.serde.serialize(req)
    sendRequest(serialized, req.id)
  }

  override def minus(a: Int, b: Int): Option[Int] = {
    val req = OperationRequest(operator = Operator.MINUS, a = a, b = b)
    val serialized = OperationRequest.serde.serialize(req)
    sendRequest(serialized, req.id)
  }

  override def divide(a: Int, b: Int): Option[Int] = {
    val req = OperationRequest(operator = Operator.DIVIDE, a = a, b = b)
    val serialized = OperationRequest.serde.serialize(req)
    sendRequest(serialized, req.id)
  }

@main
def launchClient(): Unit = {
  val clientSocket = Socket("127.0.0.1", 4444)
  val out = PrintWriter(clientSocket.getOutputStream, true)
  val in = BufferedReader(InputStreamReader(clientSocket.getInputStream))

  println(ClientStub(clientSocket, out, in).multiply(48, 4).get)
}
