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

	def params : Array[String] = try {
		fromFile(paramsFile).mkString.split(" ")(1).split("x")
	} catch{
		case e:Exception => Array("", "")
	}

	def brightness = params(0)
	def contrast = params(1)

	def transform(params:String) : FileResponse = {
		if(!smallFile.exists) {
			List("convert", f.getPath, "-scale", "600", smallFile.getPath)!!
		}

		if(params.length > 0) {
			(List("convert", smallFile.getPath)
				++ params.split(" ")
				++ List(outputFile.getPath))!!
			val os = new FileOutputStream(params)
			os.write(params.getBytes)
			os.close
		}

		return new FileResponse(outputFile.exists match {
			case true => outputFile
			case _ => smallFile
		})
	}
}
