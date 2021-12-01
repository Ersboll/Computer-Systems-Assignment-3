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

  val in = RegInit(0.U(1.W))
  val x = RegInit(0.U(5.W))
  val y = RegInit(0.U(5.W))

  io.writeEnable := false.B
  io.address := 0.U(10.W)
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
      when(x === 19.U(5.W) && y === 19.U(5.W)) { // at last pixel
        io.address := 799.U(16.W)
        io.writeEnable := true.B
        stateReg := done
      }.elsewhen( // at border
        x === 0.U(5.W) || y === 0.U(5.W) || x === 19.U(5.W) || y === 19.U(
          5.W
        )
      ) {
        // write
        io.address := Cat(0.U(9.W), x + y * 20.U(5.W)) + 400.U(16.W)
        io.writeEnable := true.B
        stateReg := write
        // increment
        when(x === 19.U(5.W)) {
          x := 0.U(5.W)
          y := y + 1.U(5.W)
        }.otherwise {
          x := x + 1.U(5.W)
        }
      }.otherwise {
        io.address := Cat(0.U(9.W), x + y * 20.U(5.W))
        in := io.dataRead(0)
        stateReg := read
        crossReg := center
      }

    }

    is(read) {
      when(in === 1.U(1.W)) {
        switch(crossReg) {
          is(center) {
            // read right pixel to in
            io.address := Cat(0.U(9.W), x + y * 20.U(5.W)) + 1.U(16.W)
            in := io.dataRead(0)
            stateReg := read
            crossReg := right
          }

          is(right) {
            // read top pixel to in
            io.address := Cat(0.U(9.W), x + y * 20.U(5.W)) - 20.U(16.W)
            in := io.dataRead(0)
            stateReg := read
            crossReg := top
          }

          is(top) {
            // read left pixel to in
            io.address := Cat(0.U(9.W), x + y * 20.U(5.W)) - 1.U(16.W)
            in := io.dataRead(0)
            stateReg := read
            crossReg := left
          }

          is(left) {
            // read bottom pixel to in
            io.address := Cat(0.U(9.W), x + y * 20.U(5.W)) + 20.U(16.W)
            in := io.dataRead(0)
            stateReg := read
            crossReg := bottom
          }

          is(bottom) {
            // write 255
            io.dataWrite := 255.U(32.W)
            io.address := Cat(0.U(9.W), x + y * 20.U(5.W)) + 400.U(16.W)
            io.writeEnable := true.B
            // increment
            when(x === 19.U(5.W)) {
              x := 0.U(5.W)
              y := y + 1.U(5.W)
            }.otherwise {
              x := x + 1.U(5.W)
            }
            stateReg := write
          }

        }
      }.otherwise { // in =/= 1
        // write 0 to address
        io.address := Cat(0.U(9.W), x + y * 20.U(5.W)) + 400.U(16.W)
        io.writeEnable := true.B
        // increment
        when(x === 19.U(5.W)) {
          x := 0.U(5.W)
          y := y + 1.U(5.W)
        }.otherwise {
          x := x + 1.U(5.W)
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
