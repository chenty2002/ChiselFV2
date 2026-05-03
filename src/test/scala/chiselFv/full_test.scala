package chiselFv

import chisel3._
import chisel3.stage.ChiselStage
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FullTest extends AnyFlatSpec with Matchers {
  behavior of "Formal"

  it should "elaborate every public assertion helper" in {
    val sv = (new ChiselStage).emitVerilog(
      new FullTestDut,
      Array("--target-dir", "target/full-test")
    )

    sv should include("module FullTestDut")
    sv should include("assert")
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
