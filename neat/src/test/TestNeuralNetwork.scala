import neural.{NeuralNetworkUtil, Neuron, NeuralNetwork}

/**
  * Created by bas on 21-12-15.
  */
object TestNeuralNetwork extends App{

  val n = new NeuralNetwork
  val x1 = new Neuron(1)
  val x2 = new Neuron(2)
  val b = new Neuron(4)
  val out = new Neuron(3)

  n.setInputNeurons(Seq(x1, x2, b))
  n.setOutputNeurons(Seq(out))

  n.createConnection(x1, out, 1.0)
  n.createConnection(x2, out, 1.0)
  n.createConnection(b, out, -1.5)

  n.setInput(1, 0, 1)
  n.evaluate
  println("n.getoutput: " + n.getOutput)
  if(NeuralNetworkUtil.treshold(n.getOutput(0)) == 0) println("Succes") else println("Failed")

  n.setInput(1, 1, 1)
  n.evaluate
  println("n.getoutput: " + n.getOutput)
  if(NeuralNetworkUtil.treshold(n.getOutput(0)) == 1) println("Succes") else println("Failed")
}
