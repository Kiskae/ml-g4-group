package neural

import org.scalatest.{FlatSpec, Matchers}

class NeuralNetworkSpec extends FlatSpec with Matchers {

  def fixture = new {
    val network = new NeuralNetwork(3, 1)
    val Seq(x1, x2, b) = network.getInputNeurons
    val out = network.getOutputNeurons.head

    network.setInputNeurons(Seq(x1, x2, b))
    network.setOutputNeurons(Seq(out))

    network.createConnection(x1, out, 1.0)
    network.createConnection(x2, out, 1.0)
    network.createConnection(b, out, -1.5)
  }

  "A neural network" should "evaluate to 0 if not everything is 1" in {
    val n = fixture.network
    n.evaluate(1, 0, 1)
    assert(n.thresholdOutput.head == 0)
  }

  it should "evaluate to 1 if everything is 1" in {
    val n = fixture.network
    n.evaluate(1, 1, 1)
    assert(n.thresholdOutput.head == 1)
  }

  it should "do something, What is the expected outcome bas?" in {
    val fix = fixture
    val hn = Neuron.between(fix.x1, fix.out)
    fix.network.addHiddenNeuron(hn)

    fix.network.createConnection(fix.x1, hn, -1.0)
    fix.network.createConnection(hn, fix.out, -1.0)
    fix.network.evaluate(1, 1, 1)
    ???
  }
}
