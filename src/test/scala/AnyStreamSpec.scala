import c04.strictness.{Stream => MyStream}
import org.scalatest.flatspec.AnyFlatSpec

class StreamSpec extends AnyFlatSpec {
  " A Stream " should " be converted to List via toList" in {
    val s = MyStream(1, 2, 3, 4)
    assert(s.toList == List(1, 2, 3, 4))
  }

  ignore should " take first 2 elements via take(2) method" in {
    val s = MyStream(1, 2, 3, 4)
    assert(s.take(2).toList == List(1, 2, 3))
  }
}
