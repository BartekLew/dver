package me.leo.dver

import java.io.{File, FileOutputStream}
import scala.io.Source._
import sys.process._

class Parameters(content : Map[String,String]) {
	def this(s:String) = this(
		s.split(";").map(p => p.split("=")).foldLeft(Map[String,String]()) ((a, p) =>
			 p.length match {
			case 0 => a
			case 1 => a + (p(0) -> "")
			case 2 => a + (p(0) -> p(1))
			case _ => throw new Exception(
				"wrong param: " + p.foldLeft("") ( (a,v) =>
					a.length match {
						case 0 => v case _ => a + "/" + v }))
	}))

	def this(f:File) = this(fromFile(f).mkString)

	def keys = content.keys
	def value(key:String) = content(key)

	def serialized : String =
		content.keys.map(k => k + "=" + content(k)).foldLeft("")((a,v) =>
			a.length match {
				case 0 => v
				case _ => a + ";" + v
		})

	def asList(m: Map[String,String] => List[String]) = m(content)	
}

trait ImageTransformator {
	def makeMinature(f:ImageFile) : Unit
	def adjustImage(p:Parameters, f:ImageFile) : Unit
}

class ImageMagick extends ImageTransformator {
	def imParams(p:Map[String,String]) : List[String] = List(
		"-brightness-contrast", p("brightness") + "x" + p("contrast")
	) ++ (p.keys.exists(_ == "more") match {
		case true => p("more").split(" ")
		case false => List()
	})

	def makeMinature(f:ImageFile) {
		List("convert", f.getPath, "-scale", "600", f.smallFile.getPath)!!
	}

	def adjustImage(p:Parameters, f:ImageFile) {
		(List("convert", f.smallFile.getPath)
			++ p.asList(imParams)
			++ List(f.outputFile.getPath))!!
	}
}

class ImageFile(f:File) {
	def this(path:String) = this(new File(path))
	def getPath = f.getPath

	def paramsFile = new File(f.getPath + "~params")
	def smallFile = new File(f.getPath + "~small")
	def outputFile = new File(f.getPath + "~out")
 
	def params : Parameters = try {
		new Parameters(paramsFile)
	} catch {
		case e:Exception => new Parameters(Map(
			"brightness"->"0", "contrast"->"0", "more"->""
		))
	}

	def transform(paramsStr:String, t:ImageTransformator) : FileResponse = {
		if(!smallFile.exists) {
			t.makeMinature(this)
		}

		if(paramsStr.length > 0) {
			try {
				val p = new Parameters(paramsStr)
			
				t.adjustImage(p, this)
				
				val os = new FileOutputStream(paramsFile)
				os.write(p.serialized.getBytes)
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
