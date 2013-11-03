import sbt._

class L3Project(info: ProjectInfo) extends DefaultProject(info) {
  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"

  override def testScalaSourcePath = "test" / "src"
  override def testResourcesPath = "test" / "resources"
}
