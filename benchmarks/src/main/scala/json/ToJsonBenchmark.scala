package json

import java.util.UUID

import io.sphere.json._
import io.sphere.json.generic._
import io.sphere.util.BaseMoney
import org.json4s.StringInput
import org.json4s.jackson._
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class ToJsonBenchmark {

  /* on local mac
  jmh:run

*** scala 2.12 ***
Benchmark                                    Mode  Cnt   Score   Error  Units
ToJsonBenchmark.listWriter                  thrpt   10   74,710 ± 1,427  ops/s
ToJsonBenchmark.seqWriter                   thrpt   10   60,558 ± 2,016  ops/s
ToJsonBenchmark.serializeCaseClassToString  thrpt   10   49,572 ± 0,700  ops/s
ToJsonBenchmark.vectorWriter                thrpt   10   62,821 ± 0,703  ops/s

*** scala 2.13 ***
Benchmark                                    Mode  Cnt   Score   Error  Units
ToJsonBenchmark.listWriter                  thrpt   10   79,194 ± 1,735  ops/s
ToJsonBenchmark.seqWriter                   thrpt   10   70,637 ± 1,383  ops/s
ToJsonBenchmark.serializeCaseClassToString  thrpt   10   48,469 ± 1,141  ops/s
ToJsonBenchmark.vectorWriter                thrpt   10   69,971 ± 2,436  ops/s
   */

  @Benchmark
  def serializeCaseClassToString(): Unit = {
    val json = toJSON[Product](JsonBenchmark.product)
    assert(json != null)
  }

  @Benchmark
  def vectorWriter(): Unit = {
    toJSON[Vector[Int]](JsonBenchmark.lotsOfIntsVector)
  }

  @Benchmark
  def listWriter(): Unit = {
    toJSON[List[Int]](JsonBenchmark.lotsOfIntsList)
  }

  @Benchmark
  def seqWriter(): Unit = {
    toJSON[Seq[Int]](JsonBenchmark.lotsOfIntsSeq)
  }

}
