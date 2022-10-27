import main.{Input, Maybe, PseudobinSerde}

import scala.util.{Failure, Success, Try}

object main {

  case class Input(data: String, offset: Int) {
    def current(n: Int): String = data.takeRight(data.length() - offset).take(n)

    def next(n: Int): Input = copy(offset = offset + n)
  }

  type Maybe[A] = Try[(A, Input)]

  trait PseudobinSerde[A] {
    def serialize(value: A): String

    def deserialize(data: Input): Maybe[A]

    def deserialize(data: String): Maybe[A] = deserialize(Input(data, 0))
  }


  object PseudobinSerde {

    val INT: PseudobinSerde[Int] = new PseudobinSerde[Int] {

      val size_space = 11

      override def serialize(value: Int): String = blank(value.toString, " ", size_space)

      override def deserialize(value: Input): Maybe[Int] =
        for {
          substring <- Try(value.data.substring(value.offset, value.offset + size_space))
          result <- Try(substring.trim.toInt)

        } yield (result, value.next(size_space))
    }

    val SHORT: PseudobinSerde[Short] = new PseudobinSerde[Short] {

      val size_space = 6

      override def serialize(value: Short): String = blank(value.toString, " ", size_space)

      override def deserialize(value: Input): Maybe[Short] =
        for {
          substring <- Try(value.data.substring(value.offset, value.offset + size_space))
          result <- Try(substring.trim.toShort)

        } yield (result, value.next(size_space))
    }

    val LONG: PseudobinSerde[Long] = new PseudobinSerde[Long] {

      val size_space = 20

      override def serialize(value: Long): String = blank(value.toString, " ", size_space)

      override def deserialize(value: Input): Maybe[Long] =
        for {
          substring <- Try(value.data.substring(value.offset, value.offset + size_space))
          result <- Try(substring.trim.toLong)

        } yield (result, value.next(size_space))
    }

    val DOUBLE: PseudobinSerde[Double] = new PseudobinSerde[Double] {

      val size_space = 24

      override def serialize(value: Double): String = blank(value.toString, " ", size_space)

      override def deserialize(value: Input): Maybe[Double] =
        for {
          substring <- Try(value.data.substring(value.offset, value.offset + size_space))
          result <- Try(substring.trim.toDouble)

        } yield (result, value.next(size_space))
    }

    val BOOLEAN: PseudobinSerde[Boolean] = new PseudobinSerde[Boolean] {

      val size_space = 5

      override def serialize(value: Boolean): String = {
        val bool = if (value == true) {
          "true"
        } else {
          "false"
        }
        blank(bool.toString, " ", size_space)
      }

      override def deserialize(value: Input): Maybe[Boolean] = {
        for {
          substring <- Try(value.data.substring(value.offset, value.offset + size_space))
          result <- Try(substring.trim.toBoolean)

        } yield (result, value.next(size_space))

      }
    }

    val STRING: PseudobinSerde[String] = new PseudobinSerde[String] {

      override def serialize(value: String): String = {
        SHORT.serialize(value.length.toShort) + value
      }

      override def deserialize(value: Input): Maybe[String] = {
        val size: String = value.current(6)
        val str: String = value.current(6 + size.trim.toInt)
        for {
          result <- Try(str.substring(6))
        } yield (result, value.next(6 + size.trim.toInt))
      }
    }


    def ARRAY[A](itemSerde: PseudobinSerde[A]): PseudobinSerde[List[A]] = new PseudobinSerde[List[A]] {

      override def serialize(value: List[A]): String = {
        val size = SHORT.serialize(value.length.toShort)
        size + value.map(itemSerde.serialize).mkString
      }

      override def deserialize(data: Input): Maybe[List[A]] = ???
    }

    def NULLABLE[A](itemSerde: PseudobinSerde[A]): PseudobinSerde[Option[A]] = new PseudobinSerde[Option[A]] {
      override def serialize(value: Option[A]): String = {
        value match {
          case Some(value: A) => "1".concat(itemSerde.serialize(value))
          case None => "0"
        }
      }

      override def deserialize(data: Input): Maybe[Option[A]] = {
        val new_input = data.next(1)
        data.data.charAt(0) match {
          case '0' => Success((None, new_input))
          case '1' =>
            for {
              (element: A, finalInput: Input) <- itemSerde.deserialize(new_input)
              result <- Success(Some(element), finalInput)
            } yield result
        }
      }
    }


    def blank(data: String, space: String, size: Int): String = {
      if (size < data.length) data else {
        space.repeat(size - data.length).concat(data)
      }
    }
  }

}

//------operationservice, request & response


trait OperationService:
  def add(a: Int, b: Int): Option[Int]
  def minus(a: Int, b: Int): Option[Int]
  def multiply(a: Int, b: Int): Option[Int]
  def divide(a: Int, b: Int): Option[Int]

/**
 * Available operators, used in operation request, sent by the client.
 */
enum Operator:
  case ADD, MINUS, MULTIPLY, DIVIDE

/**
 * Operation request sent by the client to the server.
 *
 * @param id
 *   identification of the request. It is a unique ID bound to this
 *   request. It is also named ''correlation ID''. It can be generated
 *   by using `java.util.UUID.randomUUID().toString`.
 */
case class OperationRequest(id: String=java.util.UUID.randomUUID().toString, operator: Operator, a: Int, b: Int)

object OperationRequest:
  val serde: PseudobinSerde[OperationRequest] = new PseudobinSerde[OperationRequest] {
    override def serialize(request: OperationRequest): String =
      PseudobinSerde.STRING.serialize(request.id) + PseudobinSerde.INT.serialize(request.operator.ordinal)
        + PseudobinSerde.INT.serialize(request.a) + PseudobinSerde.INT.serialize(request.b)

    override def deserialize(data: Input): Maybe[OperationRequest] = for {
      (id, input1) <- PseudobinSerde.STRING.deserialize(data)
      (ordinal, input2) <- PseudobinSerde.INT.deserialize(input1)
      (a, input3) <- PseudobinSerde.INT.deserialize(input2)
      (b, input4) <- PseudobinSerde.INT.deserialize((input3))
    } yield (OperationRequest(id, Operator.fromOrdinal(ordinal), a, b), input4)
  }

/**
 * Response sent by the server, containing the result.
 *
 * @param id correlation ID, coming from the corresponding request.
 */
case class OperationResponse(id: String, result: Option[Int])
object OperationResponse:
  val serde: PseudobinSerde[OperationResponse] = new PseudobinSerde[OperationResponse] {
    override def serialize(response: OperationResponse): String =
      PseudobinSerde.STRING.serialize(response.id) + PseudobinSerde.INT.serialize(response.result.get)

    override def deserialize(data: Input): Maybe[OperationResponse] =
      for {
        (id, input1) <- PseudobinSerde.STRING.deserialize(data)
        (result, input2) <- PseudobinSerde.INT.deserialize(input1)
      } yield (OperationResponse(id, Option(result)), input2)
  }

