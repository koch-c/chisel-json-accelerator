import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable
import scala.util.Random

class ChannelFifoSpec extends AnyFlatSpec with ChiselScalatestTester {
  "ChannelFifo" should "hold a partial output batch stable while more input arrives" in {
    test(new ChannelFifo(UInt(8.W), depth = 16)) { c =>
      c.io.out.ready.poke(false.B)
      c.io.in.valid.poke(true.B)
      c.io.in.bits.count.poke(1.U)
      for (i <- 0 until 8) c.io.in.bits.entries(i).poke((10 + i).U)
      c.io.in.ready.expect(true.B)
      c.clock.step()
      c.io.in.valid.poke(false.B)
      c.clock.step()
      c.io.out.valid.expect(true.B)
      c.io.out.bits.count.expect(1.U)
      c.io.out.bits.entries(0).expect(10.U)

      c.io.in.bits.count.poke(8.U)
      for (i <- 0 until 8) c.io.in.bits.entries(i).poke((20 + i).U)
      c.io.in.valid.poke(true.B)
      c.io.in.ready.expect(true.B)
      c.clock.step()
      c.io.in.valid.poke(false.B)
      c.clock.step(5)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.count.expect(1.U)
      c.io.out.bits.entries(0).expect(10.U)

      c.io.out.ready.poke(true.B)
      val collected = mutable.ArrayBuffer.empty[Int]
      for (_ <- 0 until 20) {
        if (c.io.out.valid.peek().litToBoolean) {
          val count = c.io.out.bits.count.peek().litValue.toInt
          for (i <- 0 until count) {
            collected += c.io.out.bits.entries(i).peek().litValue.toInt
          }
        }
        c.clock.step()
      }
      assert(collected.toSeq == Seq(10) ++ (20 until 28))
      c.io.out.valid.expect(false.B)
    }
  }

  it should "apply atomic backpressure at capacity and clear pending data on reset" in {
    test(new ChannelFifo(UInt(8.W), depth = 8)) { c =>
      c.io.out.ready.poke(false.B)
      c.io.in.valid.poke(true.B)
      c.io.in.bits.count.poke(8.U)
      for (i <- 0 until 8) c.io.in.bits.entries(i).poke(i.U)
      c.io.in.ready.expect(true.B)
      c.clock.step()
      c.io.in.bits.count.poke(1.U)
      c.io.in.ready.expect(false.B)
      c.clock.step(5)
      c.io.in.ready.expect(false.B)
      c.io.out.valid.expect(true.B)

      c.io.in.valid.poke(false.B)
      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)
      c.io.out.valid.expect(false.B)
      c.io.in.ready.expect(true.B)
    }
  }

  it should "reject invalid batch counts even when capacity is available" in {
    test(new ChannelFifo(UInt(8.W), depth = 16)) { c =>
      c.io.out.ready.poke(true.B)
      c.io.in.valid.poke(true.B)
      for (i <- 0 until 8) c.io.in.bits.entries(i).poke((40 + i).U)

      // All banks are empty and even count = 15 fits the total capacity.
      // Only count validation should prevent these batches from being accepted.
      for (count <- Seq(0, 9, 15)) {
        c.io.in.bits.count.poke(count.U)
        for (_ <- 0 until 3) {
          c.io.in.ready.expect(false.B)
          c.io.out.valid.expect(false.B)
          c.clock.step()
        }
      }

      // Both legal boundaries are ready; submit one entry to check that the
      // rejected batches neither wrote data nor advanced the bank pointers.
      c.io.in.bits.count.poke(8.U)
      c.io.in.ready.expect(true.B)
      c.io.in.bits.count.poke(1.U)
      c.io.in.ready.expect(true.B)
      c.clock.step()
      c.io.in.valid.poke(false.B)
      c.clock.step()
      c.io.out.valid.expect(true.B)
      c.io.out.bits.count.expect(1.U)
      c.io.out.bits.entries(0).expect(40.U)
      c.clock.step()
      for (_ <- 0 until 3) {
        c.io.out.valid.expect(false.B)
        c.clock.step()
      }
    }
  }

  for ((enqueueWidth, dequeueWidth, depth) <- Seq((8, 2, 16), (3, 4, 12),
      (2, 4, 12), (1, 1, 1))) {
    it should s"preserve order through randomized stalls and wraparound ($enqueueWidth/$dequeueWidth/$depth)" in {
      test(new ChannelFifo(UInt(16.W), depth, enqueueWidth, dequeueWidth)) { c =>
        val random = new Random(42)
        val expected = mutable.Queue.empty[Int]
        var pending = Seq.empty[Int]
        var next = 0
        var stalled: Option[(Int, Seq[BigInt])] = None
        var accepted = 0
        var consumed = 0
        var simultaneous = 0

        for (cycle <- 0 until 1200) {
          val draining = cycle >= 1000
          if (!draining && pending.isEmpty && random.nextInt(4) != 0) {
            val count = 1 + random.nextInt(enqueueWidth)
            pending = next until next + count
            next += count
          }
          c.io.in.valid.poke(pending.nonEmpty.B)
          c.io.in.bits.count.poke(pending.size.U)
          for (i <- 0 until enqueueWidth) {
            c.io.in.bits.entries(i).poke(pending.lift(i).getOrElse(0).U)
          }
          val ready = draining || (cycle % 100 >= 25 && random.nextBoolean())
          c.io.out.ready.poke(ready.B)

          val valid = c.io.out.valid.peek().litToBoolean
          val count = c.io.out.bits.count.peek().litValue.toInt
          val data = (0 until dequeueWidth).map(i => c.io.out.bits.entries(i).peek().litValue)
          stalled.foreach { previous =>
            assert(valid)
            assert((count, data) == previous)
          }
          stalled = if (valid && !ready) Some((count, data)) else None

          val outputFire = valid && ready
          if (outputFire) {
            assert(count >= 1 && count <= dequeueWidth)
            for (i <- 0 until count) {
              assert(expected.nonEmpty)
              assert(data(i) == BigInt(expected.dequeue()))
              consumed += 1
            }
          }
          val inputFire = pending.nonEmpty && c.io.in.ready.peek().litToBoolean
          if (inputFire) {
            expected ++= pending
            accepted += pending.size
            pending = Seq.empty
          }
          if (inputFire && outputFire) simultaneous += 1
          assert(expected.size <= depth)
          c.clock.step()
        }
        assert(pending.isEmpty && expected.isEmpty)
        assert(accepted == consumed && consumed > 100)
        if (depth > 1) assert(simultaneous > 0)
        c.io.out.valid.expect(false.B)
      }
    }
  }

  it should "preserve bundle metadata with each entry" in {
    test(new ChannelFifo(new AttributeByte, depth = 8, enqueueWidth = 2, dequeueWidth = 1)) { c =>
      c.io.out.ready.poke(false.B)
      c.io.in.valid.poke(true.B)
      c.io.in.bits.count.poke(2.U)
      for (i <- 0 until 2) {
        c.io.in.bits.entries(i).data.poke((40 + i).U)
        c.io.in.bits.entries(i).attributeId.poke(7.U)
        c.io.in.bits.entries(i).last.poke((i == 1).B)
      }
      c.io.in.ready.expect(true.B)
      c.clock.step()
      c.io.in.valid.poke(false.B)
      c.clock.step()
      for (i <- 0 until 2) {
        c.io.out.valid.expect(true.B)
        c.io.out.bits.count.expect(1.U)
        c.io.out.bits.entries(0).data.expect((40 + i).U)
        c.io.out.bits.entries(0).attributeId.expect(7.U)
        c.io.out.bits.entries(0).last.expect((i == 1).B)
        c.io.out.ready.poke(true.B)
        c.clock.step()
      }
      c.io.out.valid.expect(false.B)
    }
  }
}
