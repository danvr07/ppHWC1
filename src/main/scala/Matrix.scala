type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  //  def transpose: Matrix =
  //    m match {
  //      case Some(x) => new Matrix(Some(x.transpose))
  //      case None => new Matrix(None)
  //    }

  def transpose: Matrix =
    m match {
        case Some(x) => new Matrix(Some(x.head.indices.map(i => x.map(_(i))).toList))
        case None => new Matrix(None)
    }
    
    
  def map(f: Double => Double): Matrix =
    m match {
      case Some(x) => new Matrix(Some(x.map(_.map(f))))
      case None => Matrix(None)
    }

// am folosit inmultirea din curs

  def *(other: Matrix): Matrix = {
    m match {
      case Some(x) =>
        other.data match {
          case Some(y) =>
            if (x.head.length == y.length) {
              val res = x.map(line => other.transpose.data.get.map(col => line.zip(col).map(pair => pair._1 * pair._2).foldRight(0.0)(_ + _)))
              new Matrix(Some(res))
            } else {
              new Matrix(None)
            }
          case None => new Matrix(None)
        }
      case None => new Matrix(None)
    }
  }


  def ++(x: Double): Matrix = {
    data match {
      case Some(matrixData) if matrixData.nonEmpty =>
        val newData = matrixData.map(row => row :+ x)
        Matrix(Some(newData))
      case None => this // Returnează matricea originală dacă este None sau invalidă
    }
  }

  def -(other: Matrix): Matrix =
    m match {
      case Some(x) =>
        other.data match {
          case Some(y) =>
            if (x.length == y.length && x.head.length == y.head.length) {
              val res = (x, y).zipped.map((rowX, rowY) => rowX.lazyZip(rowY).map(_ - _) )
              new Matrix(Some(res))
            } else {
              new Matrix(None)
            }
          case None => new Matrix(None)
        }
      case None => new Matrix(None)
    }


  def data: Option[Mat] = m

  def height: Option[Int] =
    m match {
      case Some(x) => Some(x.length)
      case None => None
    }

  def width: Option[Int] =
    m match {
      case Some(x) => Some(x.head.length)
      case None => None
    }

  override def toString: String =
    m match {
      case Some(x) => x.map(_.mkString(" ")).mkString("\n")
    }
}

object Matrix {
  def apply(data: Mat): Matrix =
    new Matrix(Some(data))

  def apply(data: Option[Mat]): Matrix = new Matrix(data)

  def apply(dataset: Dataset): Matrix = {
    val data = dataset.data.map(_.map(_.toDouble))
    new Matrix(Some(data))
  }
}
