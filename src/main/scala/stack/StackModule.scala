package stack

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import java.nio.file.Paths

class StackModule(dataWidth: Int, len: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt(dataWidth.W))
    val underflow = Output(Bool())
    val overflow = Output(Bool())
    val isEmpty = Output(Bool())
    val isFull = Output(Bool())
    val popped = Output(Bool())
    val peeked = Output(Bool())
  })

  val opcode = io.in(6, 0)
  val imm = io.in(31, 7)

  val PUSH_OP = "b0100111".U(7.W)
  val POP_OP = "b1000011".U(7.W)
  val PEEK_OP = "b1000000".U(7.W)

  val stack = Reg(Vec(len, UInt(dataWidth.W)))
  val stackPointer = RegInit(0.U(log2Ceil(len + 1).W))

  val outReg = RegInit(0.U(dataWidth.W))
  val underflowReg = RegInit(false.B)
  val overflowReg = RegInit(false.B)
  val poppedReg = RegInit(false.B)
  val peekedReg = RegInit(false.B)

  outReg := 0.U
  underflowReg := false.B
  overflowReg := false.B
  poppedReg := false.B
  peekedReg := false.B

  when(opcode === PUSH_OP) {
    when(stackPointer < len.U) {
      val dataToStore = if (dataWidth <= 25) {
        imm(dataWidth - 1, 0)
      } else {
        Cat(0.U((dataWidth - 25).W), imm)
      }
      stack(stackPointer) := dataToStore
      stackPointer := stackPointer + 1.U
    }.otherwise {
      overflowReg := true.B
    }
  }.elsewhen(opcode === POP_OP) {
    when(stackPointer > 0.U) {
      stackPointer := stackPointer - 1.U
      outReg := stack(stackPointer - 1.U)
      poppedReg := true.B
    }.otherwise {
      underflowReg := true.B
    }
  }.elsewhen(opcode === PEEK_OP) {
    when(stackPointer > 0.U) {
      outReg := stack(stackPointer - 1.U)
      peekedReg := true.B
    }.otherwise {
      underflowReg := true.B
    }
  }

  io.out := outReg
  io.underflow := underflowReg
  io.overflow := overflowReg
  io.isEmpty := stackPointer === 0.U
  io.isFull := stackPointer === len.U
  io.popped := poppedReg
  io.peeked := peekedReg
}

object SVGen extends App {
  val out = Paths.get(
    "out",
    this.getClass
      .getName
      .stripSuffix("$")
  ).toString
  new ChiselStage().emitSystemVerilog(
    new StackModule(args(0).toInt, args(1).toInt),
    Array("--target-dir", out),
  )
}
