/**
 * Copyright 2011-2014 eBusiness Information, Groupe Excilys (www.ebusinessinformation.fr)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.gatling.recorder.scenario.template

import io.gatling.recorder.scenario.{ RequestElement, ScenarioElement }
import com.typesafe.scalalogging.slf4j.StrictLogging
import java.net.URL
import scala.collection.mutable.HashMap
import com.dongxiguo.fastring.Fastring.Implicits._
import io.gatling.core.util.StringHelper

case class Value(name: String, value: String)

case class SchemeHost(scheme: String, host: String)

object ExtractedUris {
  /**
   * Extract uris from scenario elements
   * @param scenarioElements - scenarion elements to extract uris from
   * @return extracted uris
   */
  private def extractUris(scenarioElements: Seq[ScenarioElement]): Seq[String] = {
    scenarioElements.collect({ case RequestElement(uri, _, _, _, _, _) => uri })
  }
}

/**
 * Extracts common URIs parts into vals. The algorithm is the following:
 *
 * group by (scheme, authority)
 * inside a group:
 *    if (count > 1) use the longer common root
 *    else use the (scheme, authority)
 * if multiple roots have the same host but different schemes/ports, create a val for the hos
 *
 * @param scenarioElements - contains uris to extracts common parts from
 */
class ExtractedUris(scenarioElements: Seq[ScenarioElement]) extends StrictLogging {
  val uris = ExtractedUris.extractUris(scenarioElements)
  val urls: Seq[URL] = uris.map(uri => new URL(uri))
  val urlGroups = new HashMap[SchemeHost, List[URL]]
  val renders = new HashMap[String, Fastring]
  var values: List[Value] = Nil

  var cnt = 1

  urls.foreach(url => urlGroups(SchemeHost(url.getProtocol, url.getHost)) = url :: urlGroups.get(SchemeHost(url.getProtocol, url.getHost)).getOrElse(Nil))

  for ((schemaHost, urls) <- urlGroups) {

    val valName = "uri" + cnt
    if (urls.size > 1 && schemesPortAreSame(urls)) {
      val paths = urls.map(url => url.getPath)
      val longestCommonPath = longestCommonRoot(paths)

      val firstUrl = urls.head
      values = new Value(valName, fast"${protocol(firstUrl)}${firstUrl.getAuthority}$longestCommonPath".toString) :: values

      for (url <- urls) {
        val restPath = url.getPath.substring(longestCommonPath.length)

        addRender(url.toString, fast"$valName + ${value(fast"${restPath}${query(url)}")}")
      }
    } else {
      values = new Value(valName, urls.head.getHost) :: values
      for (url <- urls) {
        addRender(url.toString, fast""""${protocol(url)}${user(url)}" + $valName + ${value(fast"${port(url)}${url.getPath}${query(url)}")}""")
      }
    }
    cnt += 1
  }

  private def longestCommonRoot(pathsStrs: List[String]): String = {
      def longestCommonRoot2(sa1: Array[String], sa2: Array[String]) = {
        val minLen = sa1.size.min(sa2.size)
        var p = 0
        while (p < minLen && sa1(p) == sa2(p)) {
          p += 1
        }

        sa1.slice(0, p)
      }

    val paths = pathsStrs.map(_.split("/"))
    paths.reduce(longestCommonRoot2).toSeq.mkString("/")
  }

  private def schemesPortAreSame(urlUris: List[URL]): Boolean = {
      def same(v1: Any, v2: Any) = Option(v1) == Option(v2)

    val firstUrl = urlUris.head
    urlUris.tail.forall(url => same(url.getPort, firstUrl.getPort) && same(url.getProtocol, firstUrl.getProtocol))
  }

  private def value(str: Fastring) = fast"${protectWithTripleQuotes(str)}"

  private def addRender(uri: String, render: Fastring): Unit = {
    logger.debug(s"Extracted value for: $uri")
    renders(uri) = render
  }

  private def query(url: URL): Fastring =
    if (url.getQuery == null) fast""
    else fast"?${url.getQuery}"

  private def protocol(url: URL): Fastring =
    fast"${url.getProtocol}://"

  private def user(url: URL): Fastring =
    if (url.getUserInfo == null) fast""
    else fast"${url.getUserInfo}@"

  private def port(url: URL): Fastring =
    if (url.getPort < 0) fast""
    else fast":${url.getPort}"

  def vals: List[Value] = values

  def renderUri(uri: String): Fastring = renders(uri)
}
