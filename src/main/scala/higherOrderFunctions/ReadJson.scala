package higherOrderFunctions

import spray.json._
import DefaultJsonProtocol._

case class MaskingProviders(`type`: String)
case class MaskingRule(jsonPath: String, field: String, rule: String)
case class Rule(name: String, maskingProviders: List[MaskingProviders])
case class MaskingConfig(rules: List[Rule], maskingRules: List[MaskingRule])

object MaskingConfigJsonProtocol extends DefaultJsonProtocol {
  implicit val maskingProvidersFormat: RootJsonFormat[MaskingProviders] = jsonFormat1(MaskingProviders)
  implicit val maskingRuleFormat: RootJsonFormat[MaskingRule] = jsonFormat3(MaskingRule)
  implicit val ruleFormat: RootJsonFormat[Rule] = jsonFormat2(Rule)
  implicit val maskingConfigFormat: RootJsonFormat[MaskingConfig] = jsonFormat2(MaskingConfig)
}


object ReadJson extends App{
  import MaskingConfigJsonProtocol._
  val jsonString = scala.io.Source.fromFile("/home/anand/Downloads/example_1.json").mkString
  private val maskingRules = jsonString.parseJson.convertTo[MaskingConfig]
  println(maskingRules.maskingRules)
}
