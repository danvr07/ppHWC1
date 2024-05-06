import java.util.Properties
import scala.annotation.tailrec
import scala.collection.SeqView.Sorted

class Dataset(m: List[List[String]]) {
  var data: List[List[String]] = m

  override def toString: String = data.map(_.mkString(",")).mkString("\n")

  def selectColumn(col: String): Dataset = {
    val index = data.head.indexOf(col)
    new Dataset(data.map(row => List(row(index))))

  }

    def selectColumns(cols: List[String]): Dataset = {
      val indices = cols.map(col => data.head.indexOf(col))
      new Dataset(data.map(row => indices.map(i => row(i))))
    }


  def split(percentage: Double): (Dataset, Dataset) = {
    val sortedData = data.tail.sortBy(row => row.head)

    // Calculează numărul de intrări pentru dataset-ul de evaluare
    val num = math.ceil(1 / percentage)

    def splitMethod(sortedData: List[List[String]], num: Double, counter: Int): (List[List[String]], List[List[String]]) = {
      if (sortedData.isEmpty) {
        (List(), List())
      } else {
        val (testData, trainData) = splitMethod(sortedData.tail, num, counter + 1)
        if (counter % num == 0) {
          (sortedData.head +: testData, trainData)
        } else {
          (testData, sortedData.head +: trainData)
        }
      }
    }

    val counter = 1

    val (testDataList, trainDataList) = splitMethod(sortedData, num, counter)

    val testDataWithHeader = new Dataset(data.head +: testDataList)
    val trainDataWithHeader = new Dataset(data.head +: trainDataList)


    (trainDataWithHeader, testDataWithHeader)
  }

  def drop(n: Int): Dataset = {
    new Dataset(data.drop(n))
  }


  def size: Int = data.size

  def getRows: List[List[String]] = data.tail

  def getHeader: List[String] = data.head

}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val bufferedSource = io.Source.fromFile(csv_filename)
    val data = bufferedSource.getLines().map(_.split(",").toList).toList
    bufferedSource.close()
    new Dataset(data)
  }

  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)
}
