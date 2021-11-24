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
        x := 1.U(16.W)
      }
    }

    is(write) {
      when(x === 19.U(16.W) && y === 19.U(16.W)) { // at last pixel
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
        switch(crossReg) {
          is(center) {
            // read right pixel to in
            io.address := x + y * 20.U(16.W) + 1.U(16.W)
            in := io.dataRead
            stateReg := read
            crossReg := right
          }

          is(right) {
            // read top pixel to in
            io.address := x + y * 20.U(16.W) - 20.U(16.W)
            in := io.dataRead
            stateReg := read
            crossReg := top
          }

          is(top) {
            // read left pixel to in
            io.address := x + y * 20.U(16.W) - 1.U(16.W)
            in := io.dataRead
            stateReg := read
            crossReg := left
          }

          is(left) {
            // read bottom pixel to in
            io.address := x + y * 20.U(16.W) + 20.U(16.W)
            in := io.dataRead
            stateReg := read
            crossReg := bottom
          }

          is(bottom) {
            // write 255
            io.dataWrite := 255.U(32.W)
            io.address := x + y * 20.U(16.W) + 400.U(16.W)
            io.writeEnable := true.B
            // increment
            when(x === 19.U(16.W)) {
              x := 0.U(16.W)
              y := y + 1.U(16.W)
            }.otherwise {
              x := x + 1.U(16.W)
            }
            stateReg := write
          }

        }
      }.otherwise { // in =/= 255
        // write 0 to address
        io.address := x + y * 20.U(16.W) + 400.U(16.W)
        io.writeEnable := true.B
        // increment
        when(x === 19.U(16.W)) {
          x := 0.U(16.W)
          y := y + 1.U(16.W)
        }.otherwise {
          x := x + 1.U(16.W)
        }
        stateReg := write
      }
    }

    is(done) {
      io.done := true.B
      stateReg := done
    }
  }
}
