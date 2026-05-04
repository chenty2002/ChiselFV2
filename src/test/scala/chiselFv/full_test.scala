package chiselFv

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.regex.Pattern

class FullTest extends AnyFlatSpec with Matchers {
  behavior of "Formal"

  it should "emit and verify every public assertion helper" in {
    val sv = emitVerilog()
    writeVerilog("FullTestDut.sv", sv)
    writeVerilog("FullTestDut.v", sv)

    sv should include("module FullTestDut")
    assertionMessages.foreach { message =>
      sv should include(s"Assertion failed: $message")
      assertionSignal(sv, message) should not be empty
    }

    assertionDefinition(sv, "fvAssert") should include("validData")
    assertionDefinition(sv, "fvAssert") should include("resetCounter_notChaos")
    assertionDefinition(sv, "assertAt") should include("timeSinceReset")
    assertionDefinition(sv, "assertAt") should include("32'h1")
    assertionDefinition(sv, "assertAfterNStepWhen") should include("pipe")
    assertionDefinition(sv, "assertNextStepWhen") should include("pipe_1")
    assertionDefinition(sv, "assertAlwaysAfterNStepWhen") should include("pipe_2")
    assertionDefinition(sv, "past") should include("enable")
    sv should include("wire        enable = _resetCounter_notChaos & (|_resetCounter_timeSinceReset)")
    assertionDefinition(sv, "past") should include("prevData")
    assertionDefinition(sv, "astLivenessDefault") should include("nextPending")
    assertionDefinition(sv, "astLivenessDefault") should include("7'h41")
    assertionDefinition(sv, "astLivenessBounded") should include("nextPending_1")
    assertionDefinition(sv, "astLivenessBounded") should include("3'h5")
    assertionDefinition(sv, "astRelaxedLiveness") should include("nextPending_2")
    assertionDefinition(sv, "astRelaxedLiveness") should include("3'h5")
    assertionDefinition(sv, "assertLivenessTimer") should include("timer_3")
    assertionDefinition(sv, "assertLivenessTimer") should include("3'h5")

    countOccurrences(sv, "assert property") shouldBe assertionMessages.size
    countOccurrences(sv, "$fatal") shouldBe 0
    countOccurrences(sv, "$fwrite") shouldBe 0
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

  private def writeVerilog(filename: String, sv: String): Unit = {
    val out = Path.of("verilog", filename)
    Files.createDirectories(out.getParent)
    Files.writeString(out, sv, StandardCharsets.UTF_8)
  }

  private def assertionSignal(text: String, label: String): String = {
    val pattern = Pattern.compile(
      s"""(?s)assert property \\(@\\(posedge clock\\) ([^\\)]+)\\)\\s*else\\s+\\S+\\("Assertion failed: \\Q$label\\E"""
    )
    val matcher = pattern.matcher(text)
    if (matcher.find()) matcher.group(1) else ""
  }

  private def assertionDefinition(text: String, label: String): String = {
    val signal = assertionSignal(text, label)
    val pattern = Pattern.compile(s"(?s)wire\\s+\\Q$signal\\E\\s*=\\s*(.*?);")
    val matcher = pattern.matcher(text)
    if (matcher.find()) matcher.group(1) else ""
  }

  private def emitVerilog(): String = {
    circt.stage.ChiselStage.emitSystemVerilog(
      new FullTestDut,
      Array("--target-dir", "verilog"),
      Array("--emit-chisel-asserts-as-sva")
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
