import main.Input

import scala.util.{Failure, Success, Try}
import java.net.*
import java.io.*
import java.io.BufferedReader
import java.io.PrintWriter
import java.net.ServerSocket
import java.net.Socket

class ServerStub() extends OperationService:
  override def add(a: Int, b: Int): Option[Int] = {
    Option(a + b)
  }

  override def minus(a: Int, b: Int): Option[Int] = {
    Option(a - b)
  }

  override def multiply(a: Int, b: Int): Option[Int] = {
    Option(a * b)
  }
  override def divide(a: Int, b: Int): Option[Int] = {
    Option(a / b)
  }

@main
def launchServer(): Unit = {
  val serverSocket = ServerSocket(4444)
  val clientSocket = serverSocket.accept
  val out = new PrintWriter(clientSocket.getOutputStream, true)
  val in = new BufferedReader(InputStreamReader(clientSocket.getInputStream))
  val stub = ServerStub()

  val request = in.readLine 
  OperationRequest.serde.deserialize(request) match { 
    case Success((value: OperationRequest, input: Input)) =>
      val result = value.operator match {
        case Operator.ADD => stub.add(value.a, value.b)
        case Operator.MINUS => stub.minus(value.a, value.b)
        case Operator.MULTIPLY => stub.multiply(value.a, value.b)
        case Operator.DIVIDE => stub.divide(value.a, value.b)
      }
      val response = OperationResponse.serde.serialize(OperationResponse(value.id, result))
      out.println(response) 
    case Failure(e) => println(e)
  }
}