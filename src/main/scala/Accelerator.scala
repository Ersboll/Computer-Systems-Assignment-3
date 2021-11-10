import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt(16.W))
    val dataRead = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite = Output(UInt(32.W))

  })

  val idle :: increment :: readIn :: cross :: write :: done :: Nil = Enum(6)
  val stateReg = RegInit(idle)

  val addressReg = RegInit(0.U(16.W))
  val inReg = RegInit(0.U(32.W))
  val outReg = RegInit(0.U(32.W))
  val x = RegInit(0.U(16.W))
  val y = RegInit(0.U(16.W))
  val count = RegInit(0.U(2.W))

  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := outReg
  io.done := false.B

  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := write
        addressReg := 0.U(16.W)
        x := 0.U(16.W)
        y := 0.U(16.W)
        outReg := 0.U(32.W)
      }
    }

    is(write) {
      when(addressReg === 399.U(16.W)) {
        io.address := addressReg + 400.U(16.W)
        io.writeEnable := true.B
        io.done := true.B
        stateReg := done
      }.otherwise {
        stateReg := increment

        // write
        io.address := addressReg + 400.U(16.W)
        io.writeEnable := true.B

        // increment
        addressReg := addressReg + 1.U(16.W)
        x := x + 1.U(16.W)
        when(x === 20.U(16.W)) {
          x := 0.U(16.W)
          y := y + 1.U(16.W)
        }
      }

    }

    is(increment) {
      when(
        x === 0.U(16.W) || y === 0.U(16.W) || x === 19.U(16.W) || y === 19.U(
          16.W
        )
      ) {
        outReg := 0.U(32.W)
        stateReg := write
      }.otherwise {
        io.address := addressReg
        inReg := io.dataRead
        stateReg := readIn
      }
    }

    is(readIn) {
      when(inReg === 255.U(32.W)) {
        count := 0.U(2.W)
        io.address := addressReg + 1.U(16.W)
        inReg := io.dataRead
        stateReg := cross
      }.otherwise {
        outReg := 0.U(32.W)
        stateReg := write
      }
    }

    is(cross) {
      when(inReg =/= 255.U(32.W)) {
        outReg := 0.U(32.W)
        stateReg := write
      }.otherwise {
        switch(count) {
          is("b00".U) {
            io.address := addressReg - 1.U(16.W)
            inReg := io.dataRead
            count := count + 1.U(2.W)
          }
          is("b01".U) {
            io.address := addressReg + 20.U(16.W)
            inReg := io.dataRead
            count := count + 1.U(2.W)
          }
          is("b10".U) {
            io.address := addressReg - 20.U(16.W)
            inReg := io.dataRead
            count := count + 1.U(2.W)
          }
          is("b11".U) {
            outReg := 255.U(32.W)
            stateReg := write
          }
        }
      }
    }

    is(done) {
      io.done := true.B
      stateReg := done
    }
  }
}
