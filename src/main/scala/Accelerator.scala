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

  val idle :: write :: read :: done :: Nil = Enum(4)
  var center :: right :: top :: left :: bottom :: Nil = Enum(5)
  val stateReg = RegInit(idle)
  val crossReg = RegInit(center)

  // val addressReg = RegInit(400.U(16.W))
  val in = RegInit(0.U(32.W))
  val x = RegInit(0.U(16.W))
  val y = RegInit(0.U(16.W))

  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := 0.U(32.W)
  io.done := false.B

  switch(stateReg) {
    is(idle) {
      when(io.start) {
        // write
        io.address := 400.U(16.W)
        io.writeEnable := true.B
        stateReg := write
        // increment
        when(x === 19.U(16.W)) {
          x := 0.U(16.W)
          y := y + 1.U(16.W)
        }.otherwise {
          x := x + 1.U(16.W)
        }
      }
    }

    is(write) {
      when(x + y * 20.U(16.W) === 399.U(16.W)) { // at last pixel
        // write
        io.address := 799.U(16.W)
        io.writeEnable := true.B
        io.done := true.B
        stateReg := done
      }.elsewhen( // at border
        x === 0.U(16.W) || y === 0.U(16.W) || x === 19.U(16.W) || y === 19.U(
          16.W
        )
      ) {
        // write
        io.address := x + y * 20.U(16.W) + 400.U(16.W)
        io.writeEnable := true.B
        stateReg := write
        // increment
        when(x === 19.U(16.W)) {
          x := 0.U(16.W)
          y := y + 1.U(16.W)
        }.otherwise {
          x := x + 1.U(16.W)
        }
      }.otherwise {
        io.address := x + y * 20.U(16.W)
        in := io.dataRead
        stateReg := read
        crossReg := center
      }

    }

    is(read) {
      when(in === 255.U(32.W)) {
        in := io.dataRead
        stateReg := read
        switch(crossReg) {
          is(center) {
            io.address := x + y * 20.U(16.W) + 1.U(16.W)
            crossReg := right
          }

          is(right) {
            io.address := x + y * 20.U(16.W) - 20.U(16.W)
            crossReg := top
          }

          is(top) {
            io.address := x + y * 20.U(16.W) - 1.U(16.W)
            crossReg := left
          }

          is(left) {
            io.address := x + y * 20.U(16.W) + 20.U(16.W)
            crossReg := bottom
          }

          is(bottom) {
            // write
            io.dataWrite := 255.U(32.W)
            io.address := x + y * 20.U(16.W) + 400.U(16.W)
            io.writeEnable := true.B
            stateReg := write
            // increment
            when(x === 19.U(16.W)) {
              x := 0.U(16.W)
              y := y + 1.U(16.W)
            }.otherwise {
              x := x + 1.U(16.W)
            }
          }

        }
      }.otherwise {
        // write
        io.address := x + y * 20.U(16.W) + 400.U(16.W)
        io.writeEnable := true.B
        stateReg := write
        // increment
        when(x === 19.U(16.W)) {
          x := 0.U(16.W)
          y := y + 1.U(16.W)
        }.otherwise {
          x := x + 1.U(16.W)
        }
      }
    }

    is(done) {
      io.done := true.B
      stateReg := done
    }
  }
}
