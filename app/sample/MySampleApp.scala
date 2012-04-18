package sample

class ClassSampleApp {

  this: TraitController
                        with TraitDbLayer
                        with TraitExternalWs
                        with TraitHandler =>

  def wholeThing: String = controllerPiece + "; " +
                            dbLayerPiece + "; " +
                            externalWsPiece + "; " +
                            handlerPiece

}
