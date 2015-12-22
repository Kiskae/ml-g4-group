package neural

import org.scalatest.{FlatSpec, Matchers}

class NeuralNetworkSpec extends FlatSpec with Matchers {

  def fixture = new {
    val network = new NeuralNetwork
    val x1 = network.newNeuron
    val x2 = network.newNeuron
    val b = network.newNeuron
    val out = network.newNeuron

    network.setInputNeurons(Seq(x1, x2, b))
    network.setOutputNeurons(Seq(out))

    network.createConnection(x1, out, 1.0)
    network.createConnection(x2, out, 1.0)
    network.createConnection(b, out, -1.5)
  }

  "A neural network" should "evaluate to 0 if not everything is 1" in {
    val n = fixture.network
    n.setInput(1, 0, 1)
    n.evaluate
    assert(NeuralNetworkUtil.threshold(n.getOutput.head) == 0)
  }

  it should "evaluate to 1 if everything is 1" in {
    val n = fixture.network
    n.setInput(1, 1, 1)
    n.evaluate
    assert(NeuralNetworkUtil.threshold(n.getOutput.head) == 1)
  }

  it should "do something, What is the expected outcome bas?" in {
    val fix = fixture
    val hn = fix.network.newNeuron
    fix.network.addHiddenNeuron(hn)

    fix.network.createConnection(fix.x1, hn, -1.0)
    fix.network.createConnection(hn, fix.out, -1.0)
    fix.network.evaluate
    ???
  }
}
