import main.{Input, PseudobinSerde}

import scala.util.{Failure, Success, Try}
import org.scalatest.funsuite.AnyFunSuite


class SerializationTest extends AnyFunSuite {


  test("toPseudobin and fromPseudobin for SHORT = 42") {
    val str = PseudobinSerde.SHORT.serialize(42)
    assert(str == "    42")
    val short = PseudobinSerde.SHORT.deserialize(str)
    assert(short == Success(42, Input(str, 6)))
  }

  test("toPseudobin and fromPseudobin for SHORT = -42") {
    val str = PseudobinSerde.SHORT.serialize(-42)
    assert(str == "   -42")
    val short = PseudobinSerde.SHORT.deserialize(str)
    assert(short == Success(-42, Input(str, 6)))
  }

  test("toPseudobin and fromPseudobin for INT = 42") {
    val str = PseudobinSerde.INT.serialize(42)
    assert(str == "         42")
    val int = PseudobinSerde.INT.deserialize(str)
    assert(int == Success(42, Input(str, 11)))
  }


  test("toPseudobin and fromPseudobin for LONG = 42") {
    val str = PseudobinSerde.LONG.serialize(42)
    assert(str == "                  42")
    val long = PseudobinSerde.LONG.deserialize(str)
    assert(long == Success(42, Input(str, 20)))
  }

  test("toPseudobin and fromPseudobin for BOOLEAN = true") {
    val str = PseudobinSerde.BOOLEAN.serialize(true)
    assert(str == " true")
    val boolean = PseudobinSerde.BOOLEAN.deserialize(str)
    assert(boolean == Success(true, Input(str, 5)))
  }

  test("StoPseudobin and fromPseudobin for BOOLEAN = False") {
    val str = PseudobinSerde.BOOLEAN.serialize(false)
    assert(str == "false")
    val boolean = PseudobinSerde.BOOLEAN.deserialize(str)
    assert(boolean == Success(false, Input(str, 5)))
  }

  test("toPseudobin and fromPseudobin for String = \"hello\"") {
    val str = PseudobinSerde.STRING.serialize("hello")
    assert(str == "     5hello")
    val string = PseudobinSerde.STRING.deserialize(str)
    assert(string == Success("hello", Input(str, 11)))
  }

  test("toPseudobin and fromPseudobin for String = ") {
    val str = PseudobinSerde.STRING.serialize("")
    assert(str == "     0")
    val string = PseudobinSerde.STRING.deserialize(str)
    assert(string == Success("", Input(str, 6)))
  }

  test("toPseudobin and fromPseudobin for DOUBLE = 42.24") {
    val str = PseudobinSerde.DOUBLE.serialize(42.24)
    assert(str == "                   42.24")
    val double = PseudobinSerde.DOUBLE.deserialize(str)
    assert(double == Success(42.24, Input(str, 24)))
  }

  test("toPseudobin and fromPseudobin for Nullable: Some(\"hello\")") {
    val str = PseudobinSerde.NULLABLE(PseudobinSerde.STRING).serialize(Some("hello"))
    assert(str == "1     5hello")
    val string = PseudobinSerde.NULLABLE(PseudobinSerde.STRING).deserialize(str)
    assert(string == Success(Some("hello"), Input(str, 12)))
  }

  test("toPseudobin and fromPseudobin for Nullable: None") {
    val str = PseudobinSerde.NULLABLE(PseudobinSerde.STRING).serialize(None)
    assert(str == "0")
    val string = PseudobinSerde.NULLABLE(PseudobinSerde.STRING).deserialize(str)
    assert(string == Success(None, Input(str, 1)))
  }

}