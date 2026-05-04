package chiselFv

import chisel3._
import chisel3.stage.ChiselStage
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.regex.Pattern
import scala.annotation.nowarn

class FullTest extends AnyFlatSpec with Matchers {
  behavior of "Formal"

  it should "emit and verify every public assertion helper" in {
    val sv = emitVerilog()
    val out = Path.of("verilog", "FullTestDut.v")
    Files.createDirectories(out.getParent)
    Files.writeString(out, sv, StandardCharsets.UTF_8)

    sv should include("module FullTestDut")
    assertionMessages.foreach { message =>
      sv should include(s"Assertion failed: $message")
    }
    sv should include("nextTimer <= 7'h40")
    sv should include("nextTimer_1 <= 3'h4")
    sv should include("nextTimer_2 <= 3'h4")
    sv should include("nextTimer_3 <= 3'h4")
    countOccurrences(sv, "$fatal") shouldBe assertionMessages.size
    countOccurrences(sv, "assert property") shouldBe 0
  }

  private val assertionMessages = Seq(
    "fvAssert",
    "assertAt",
    "assertAfterNStepWhen",
    "assertNextStepWhen",
    "assertAlwaysAfterNStepWhen",
    "past",
    "astLivenessDefault",
    "astLivenessBounded",
    "astRelaxedLiveness",
    "assertLivenessTimer"
  )

  private def countOccurrences(text: String, needle: String): Int = {
    Pattern.compile(Pattern.quote(needle)).matcher(text).results().count().toInt
  }

  @nowarn("cat=deprecation")
  private def emitVerilog(): String = {
    (new ChiselStage).emitVerilog(
      new FullTestDut,
      Array("--target-dir", "verilog")
    )
  }
}

private class FullTestDut extends Module with Formal {
  val io = IO(new Bundle {
    val req = Input(Bool())
    val resp = Input(Bool())
    val data = Input(UInt(4.W))
    val out = Output(UInt(4.W))
  })

  val symbolic = anyconst(4)
  val init = initialReg(4, 3)
  val prevData = RegNext(io.data, 0.U)
  val validData = io.data =/= 15.U

  init.io.in := io.data
  io.out := init.io.out ^ symbolic

  fvAssert(validData, "fvAssert")
  assertAt(1.U, validData, "assertAt")
  assertAfterNStepWhen(io.req, 2, validData, "assertAfterNStepWhen")
  assertNextStepWhen(io.req, validData, "assertNextStepWhen")
  assertAlwaysAfterNStepWhen(io.req, 2, validData, "assertAlwaysAfterNStepWhen")

  past(io.data, 1) { pastData =>
    fvAssert(pastData === prevData, "past")
  }

  astLiveness(io.req, io.resp, "astLivenessDefault")
  astLiveness(io.req, io.resp, 4, "astLivenessBounded")
  astRelaxedLiveness(io.req, io.resp, 4, "astRelaxedLiveness")
  assertLivenessTimer(io.req, io.resp, 4, "assertLivenessTimer")
}
