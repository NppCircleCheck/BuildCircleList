package config

import java.io.FileInputStream
import org.yaml.snakeyaml.Yaml
import java.io.InputStream
import java.io.File
import org.yaml.snakeyaml.constructor.Constructor
import scala.beans.BeanProperty
import scala.collection.JavaConversions._

class Config {
  @BeanProperty var template: String = ""
  @BeanProperty var output: String = ""
  @BeanProperty var base: String = ""
  @BeanProperty var input: java.util.List[String] = null
  @BeanProperty var double: java.util.List[java.util.Map[String, Object]] = null
  /**
   * 二人以上が同一サークルをチェックした場合のユーザ名
   */
  def getDoubleName(user1: String, user2: String): Option[String] = {
    val immutable = double.toList.map(_.toMap)
    (immutable.find(d => {
      d.get(user1).isDefined
    }) match {
      case Some(ud) => ud.get(user1)
      case None => None
    }) match {
      case Some(ud) => Option(ud.asInstanceOf[java.util.Map[String, String]].get(user2))
      case None => None
    }
  }

  /**
   * 入力YAMLのパスリスト
   */
  def getInputPaths = {
    val immutable = input.toList
    immutable.map(in =>
      base + File.separator + in
    )
  }
}

/**
 * configの読み込み
 */
object LoadConfig {
  val config = loadConfig
  def loadConfig: Config = {
    val input: InputStream = new FileInputStream(new File("conf\\config.yml"))
    val yaml: Yaml = new Yaml(new Constructor(classOf[Config]))
    yaml.load(input).asInstanceOf[Config]
  }
}