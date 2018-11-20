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

	def forPreasent(key:String) : String = value(key) match {
		case "" => ""
		case _ => key + "=" + value(key) + "\n"
	}
}

trait ImageTransformator {
	def minatureName(base:String) : String
	def outputName(base:String) : String

	def makeMinature(f:ImageFile) : Unit
	def adjustImage(p:Parameters, f:ImageFile) : Unit
	def params : List[String]
}

class ImageMagick extends ImageTransformator {
	def minatureName(base:String) = base + "~small"
	def outputName(base:String) = base + "~out"

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

	def params = List("contrast", "brightness", "more")
}	

class RawTherapee extends ImageTransformator {
	def minatureName(base:String) = base + ".jpg"
	def outputName(base:String) = base + ".out.jpg"
	def resize = "[Resize]\nEnabled=true\nwidth=300"

	def makeMinature(f:ImageFile) {
		println("rawtherapee...")
		val os = new FileOutputStream(f.getPath + ".pp3")
		os.write(resize.getBytes)
		os.close
		
		println(List("rawtherapee-cli", "-s", "-o", f.smallFile.getPath,
			"-c", f.getPath)!!)
	}

	def configOf(p:Parameters) : String =
		"[Exposure]\n" + params.foldLeft("") ((a,k) =>
			a + p.forPreasent(k)
		)

	def adjustImage(p:Parameters, f:ImageFile) {
		val os = new FileOutputStream(f.getPath + ".pp3")
		os.write((configOf(p) + resize).getBytes)
		os.close
		
		println("rawtherapee...")
		println(List("rawtherapee-cli", "-Y", "-o", f.outputFile.getPath, "-s", "-c", f.getPath)!!)
	}

	def params = List(
		"Auto", "Brightness", "Contrast", "Saturation", "Compensation", "Black"
	)
}

class ImageFile(f:File, t:ImageTransformator) {
	def this(path:String, t:ImageTransformator) = this(new File(path), t)
	def getPath = f.getPath

	def paramsFile = new File(f.getPath + "~params")
	def smallFile = new File(t.minatureName(f.getPath))
	def outputFile = new File(t.outputName(f.getPath))
 
	def params : Parameters = try {
		new Parameters(paramsFile)
	} catch {
		case e:Exception => new Parameters(t.params.foldLeft(Map[String,String]())(
			(a,v) => a + (v->"")
		))
	}

	def paramNames = t.params

	def transform(paramsStr:String) : FileResponse = {
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
