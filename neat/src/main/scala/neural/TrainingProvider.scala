package neural

import agent.PlayerInputProvider
import neural.TrainingProvider.Wrapper

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TrainingProvider(private var providerProducer: () => PlayerInputProvider) {
  private val toClearWrappers: mutable.Buffer[Wrapper[_]] = ArrayBuffer()

  def setProvider(newProviderProducer: () => PlayerInputProvider) = {
    //Clear all previous wrappers to prevent memory leaks
    toClearWrappers.foreach(_.value = None)
    toClearWrappers.clear()

    providerProducer = newProviderProducer
  }

  def provider: PlayerInputProvider = {
    val wrapper = TrainingProvider.cache.get()
    wrapper.value match {
      case Some(provider) => provider
      case None =>
        toClearWrappers :+ wrapper
        val newProvider = providerProducer()
        wrapper.value = Some(newProvider)
        newProvider
    }
  }
}

object TrainingProvider {

  private case class Wrapper[T](var value: Option[T])

  private val cache: ThreadLocal[TrainingProvider.Wrapper[PlayerInputProvider]] =
    new ThreadLocal[Wrapper[PlayerInputProvider]] {
      override def initialValue(): Wrapper[PlayerInputProvider] = Wrapper(None)
    }
}
