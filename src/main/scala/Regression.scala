import javax.transaction.xa.Xid
import scala.annotation.tailrec

object Regression {


  def regression(dataset_file: String,
                 attribute_columns: List[String],
                 value_column: String,
                 test_percentage: Double,
                 alpha: Double,
                 gradient_descent_steps: Int): (Matrix, Double) = {

    // Citirea datelor
    val dataset = Dataset(dataset_file)

    val selectedDataset = dataset.selectColumns(attribute_columns :+ value_column)

    val (train, test) = selectedDataset.split(test_percentage)

    val X_data = train.selectColumns(attribute_columns)
    //println(X_data)
    val Y_data = train.selectColumn(value_column)
    val X_data_without_first_line = X_data.drop(1)
    val y_data_without_first_line = Y_data.drop(1)
    val matrix = Matrix(X_data_without_first_line)

    val X = matrix ++ 1.0
    val Y = Matrix(y_data_without_first_line)

    val m = X.height.get
    val n = X.width.get

    val W = Matrix(List.fill(n)(List.fill(1)(0.0)))

    @tailrec
    def gradientDescent(X: Matrix, Y: Matrix, W: Matrix, alpha: Double, steps: Int): Matrix = {
      if (steps <= 0) W
      else {
        val estimations: Matrix = X * W
        val error = estimations - Y
        val gradient = (X.transpose * error).data.get.map(x => x.map(_ / m))

        val updatedWeights = W - Matrix(gradient.map(x => x.map(_ * alpha)))
        gradientDescent(X, Y, updatedWeights, alpha, steps - 1)
      }
    }

    val finalWeights = gradientDescent(X, Y, W, alpha, gradient_descent_steps)

    val testX_data = Matrix(test.selectColumns(attribute_columns).drop(1)) ++ 1.0

    val testY_data = Matrix(test.selectColumn(value_column).drop(1))


    val estimations = testX_data * finalWeights

    val error = estimations - testY_data
    val sum = error.data.get.foldLeft(0.0)((acc, x) => acc + x.head)
    val estimatedError = sum / error.height.get


    (finalWeights, estimatedError)
  }


  def main(args: Array[String]): Unit = {
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}