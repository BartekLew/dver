package me.leo.dver

import java.io.{File, FileOutputStream}
import scala.io.Source._
import sys.process._

class ImageFile(f:File) {
	def this(path:String) = this(new File(path))
	def getPath = f.getPath

	def paramsFile = new File(f.getPath + "~params")
	def smallFile = new File(f.getPath + "~small")
	def outputFile = new File(f.getPath + "~out")


	def deserialize(s:String) : Map[String,String] =
		s.split(";").map(p => p.split("="))
			.foldLeft(Map[String,String]()) ((a, p) => p.length match {
				case 0 => a
				case 1 => a + (p(0) -> "")
				case 2 => a + (p(0) -> p(1))
				case _ => throw new Exception("wrong param: " + p.foldLeft("") ((a,v) => a.length match { case 0 => v case _ => a + "/" + v }))
			})

	def serialize (p:Map[String,String]) : String =
		p.keys.map(k => k + "=" + p(k)).foldLeft("")((a,v) => a.length match {
			case 0 => v
			case _ => a + ";" + v
		})
 
	def params : Map[String,String] = try {
		deserialize(fromFile(paramsFile).mkString)
	} catch {
		case e:Exception => Map("brightness"->"0", "contrast"->"0", "more"->"")
	}

	def imParams(p:Map[String,String]) : List[String] = List(
		"-brightness-contrast", p("brightness") + "x" + p("contrast")
	) ++ (p.keys.exists(_ == "more") match {
		case true => p("more").split(" ")
		case false => List()
	})

	def transform(paramsStr:String) : FileResponse = {
		if(!smallFile.exists) {
			List("convert", f.getPath, "-scale", "600", smallFile.getPath)!!
		}

		if(paramsStr.length > 0) {
			try {
				val p = deserialize(paramsStr)
			
				(List("convert", smallFile.getPath)
					++ imParams(p)
					++ List(outputFile.getPath))!!
				val os = new FileOutputStream(paramsFile)
				os.write(serialize(p).getBytes)
				os.close
			} catch {
				case e:Exception => println(new StackTrace(e).toString)
			}
		}

		return new FileResponse(outputFile.exists match {
			case true => outputFile
			case _ => smallFile
		})
	}
}
