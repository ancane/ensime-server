// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import org.ensime.vfs.EnsimeVFS

class DocJarReadingSpec
    extends EnsimeSpec
    with SharedEnsimeConfigFixture
    with SharedEnsimeVFSFixture {

  val original = EnsimeConfigFixture.DocsTestProject

  "DocJarReading" should "serve entries from jar files" in {
    withEnsimeConfig { c =>
      withVFS { implicit fs =>

        val reader = new DocJarReading {
          val config = c
          val vfs = fs
        }

        val content = reader.docJarContent("scala-library-" + c.scalaVersion + "-javadoc.jar", "index.html")
        content shouldBe defined
      }
    }
  }
}
