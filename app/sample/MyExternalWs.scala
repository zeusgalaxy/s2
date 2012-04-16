package sample

trait MyExternalWsLike {
  def getX: Int
  def getY: String
}

class MyExternalWsReal extends MyExternalWsLike {
  def getX = 1
  def getY = "real"
}

class MyExternalWsMock extends MyExternalWsLike {
  def getX = 2
  def getY = "mock"
}
