import com.fasterxml.jackson.databind.ObjectMapper

object HelloJackson {

  def main(args: Array[String]): Unit = {
    val mapper = new ObjectMapper()
    println(mapper.getClass)
  }

}
