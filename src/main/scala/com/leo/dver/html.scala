package me.leo.dver

import java.io.File

import scala.io.Source._
import scala.sys.process._

class Tag(name:String, args:Map[String,String], value:Option[String]) {

	def this(name:String) = this(name, Map(), None)
	def this(name:String, args:Map[String,String]) =
		this(name, args, None)
	def this(name:String, value:String) = this(name, Map(), Some(value))
	def this(name:String, children:List[Tag]) = this(name, Map(),
		Some(children.foldLeft(""){(a, v)=> a + v.toString})
	)

	private def argsStr = args.size match {
		case 0 => ""
		case _ => " " + args.keys.fold ("") {
			(acc, v) => acc + v + "=\"" + args(v) +	"\" "
		}
	}

	private def closingStr = value match {
		case Some(s) => ">" + s + "</" + name + ">"
		case None => "/>"
	}

	override def toString =
		"<" + name + argsStr + closingStr
}

class BList(items:List[String]) extends Tag("ul",
	items.foldLeft("") { (x, r) =>
		x + "<li>" + r + "</li>"
	}
)

class Link(url:String, text:String) extends Tag(
	"a",Map("href" -> url), Some(text)
)

class StyledLink(url:String, style:String, text:String) extends Tag(
	"a",Map("href" -> url, "style"->style), Some(text)
)

class Encoding(v:String) extends Tag("meta", Map("charset" -> v))

class CssBlock(rules:Map[String,String]) extends Tag("style", Map(),
	Some(rules.keys.foldLeft("") {
		(a, k) => a + "\n" + k + "{" + rules(k) + "}"
	}
))

class Button(text:String, action:String) extends Tag(
	"button", Map("onclick"->action), Some(text)
)

trait Iface {
	def tags:List[Tag]
	def js:String

	def merge(ifaces:List[Iface]): List[Tag] = ifaces.size match {
		case 0 => this.tags
		case _ => this.tags ++ List(new Tag("br")) ++ 
				ifaces.head.merge(ifaces.tail)
	}

	def jsFun(name:String, body:String) =
		"function " + name + "() {" + body + "}"

	def jsFun(name:String, args:String, body:String) =
		"function " + name + "(" + args + ") { " + body + "}"

	def jsId(id:String) = "document.getElementById(\"" + id + "\")"

	def fileContent(f:File) = f.exists match {
		case true => xml.Utility.escape(fromFile(f, "utf-8").mkString)
		case false => ""
	}
}

class ScriptResult(path:String) extends Iface {
	def tags : List[Tag] = List(
		new Tag("div", Map(), Some(path!!))
	)

	def js = ""
}
 
class DirListing(f:File) extends Iface {
	def openLinks(f:File) : String =
		f.isDirectory match {
			case true => new Link(
				"/r/" + f.getPath, f.getName
			).toString

			case false => new Link(
				"/R/" + f.getPath, f.getName
			).toString + " " + new Link (
				"/r/" + f.getPath, "[edit]"
			).toString
		}

	def listing(f:File) : List[File] = f.getPath match {
		case "/" | "." => f.listFiles.toList.sorted
		case _ => List(new File(f.getPath+"/..")) ++ f.listFiles.toList.sorted
	}

	def fileLinks(f:File) : String =
		openLinks(f) +
		new StyledLink("/d/" + f.getPath,
			"margin-left:1ex;font-size:0.8em;font-weight:bold;",
			"x").toString

	def tags = List(
		new Tag("h3", Map(), Some("Directory: " + f.getPath)),
		new BList(listing(f).map(fileLinks))
	)

	def js = ""
}

class FileCreator(cwd:File) extends Iface {
	def tags = List(
		new Tag("input", Map("type"->"text", "id"->"newfilename")),
		new Button("Create", "newFile()")
	)

	def js = new JsLocation().set(
		new Js().literal("/r/" + cwd.getPath + "/")
			.jsId("newfilename", "value")
	).asFun("newFile").code 
}

class FileUploader(cwd:File) extends Iface {
	def tags = List(
		new Tag("input", Map("type"->"file", "id"->"upFile")),
		new Button("Upload", "uploadFile()")
	)

	def js = (new JsVar("r").set(new Js().jsId("upFile", "files[0]")) +
		new JsHttp("POST", new Js().literal("/w/" + cwd.getPath + "/")
				.jsVar("r.name"), new Js("r"),
				new Js("window.location.reload(false)")
		)).asFun("uploadFile").code
	
}

class FileEditor(cwd:File) extends Iface {
	def tags = List(
		new Tag("h3", Map(), Some("File: " + cwd.getPath + " " +
			new Tag("a", Map("href" -> ("/r/" + cwd.getParent)), Some("(parent directory)")).toString
		)),
		new Tag("textarea", Map("id" -> "texted",
				"onkeypress"->"onKey(event)"),
			Some(fileContent(cwd))),
		new Tag("br"),
		new Button("save", "updateFile()")
	)

	def js = new JsHttp("POST", new Js().literal("/w/" + cwd.getPath),
			new Js().jsId("texted", "value"),
			new Js("alert(\"done\");")
		).asFun("updateFile").code +
		new Js().cond(new Js("ev.keyCode == 9"),
			new Js().call("ev.preventDefault", List()) +
			new JsVar("ed").set(new Js().jsId("texted")) +
			new JsVar("pos").set(new Js("ed.selectionStart")) +
			new Js("ed.value").set(
				new Js("ed.value.substring(0, pos)")
					.literal("\\t").jsVar(
		"ed.value.substring(ed.selectionEnd, ed.value.length)"
				)
			) +
			new Js("ed.selectionStart = ed.selectionEnd = pos+1;")
		).asFun("onKey", "ev").code

}

class Shell(path:String) extends Iface {
	def js = ((new JsHttp("GET",
			new Js().literal("/R/" + path + "/box.out"),
			new Js(),
			new JsVar("o").set(new Js().jsId("sh_out")) +
			new Js().cond(new Js("this.responseText.length != o.value.length"),
				new Js().jsVar("o.value").set(
					new Js().jsVar("this.responseText")
				) +
				new Js().jsVar("o.scrollTop").set(
					new Js().jsVar("o.scrollHeight")
				)
			)
		)+new JsHttp("GET",
			new Js().literal("/R/" + path + "/box.err"),
			new Js(),
			new JsVar("e").set(new Js().jsId("sh_err")) +
			new Js().cond(new Js("this.responseText.length != e.value.length"),
				new Js().jsVar("e.value").set(
					new Js().jsVar("this.responseText")
				) +
				new Js().jsVar("e.scrollTop").set(
					new Js().jsVar("e.scrollHeight")
				)
			)
		)).asFun("refresh_output") +
		new Js().cond(new Js("ev.keyCode == 13"),
			new JsVar("cmd").set(new Js().jsId("sh_in", "value")) +
			new JsVar("out").set(new Js().jsId("sh_out")) +
			new Js("refresh_output();") +
			new JsHttp("POST", new Js().literal("/sh/" + path),
				new Js().jsVar("cmd"),
				new Js("setInterval(refresh_output, 2000);")
		)).asFun("sh_cmd", "ev")).code

	def tags = List(
		new Tag("h4", Map(), Some("shell:")),
		new Tag("input", Map(
			"type"->"text", "id"->"sh_in",
			 "onkeypress"->"sh_cmd(event)"
		)),
		new Tag("textarea readonly", Map("id"->"sh_out"),
			Some(fileContent(new File(path + "/box.out")))
		),
		new Tag("textarea readonly", Map("id"->"sh_err"),
			Some(fileContent(new File(path + "/box.err")))
		),
		new Tag("script", Map(), Some(
			(new JsVar("o").set(new Js().jsId("sh_out"))+
			new Js("o.scrollTop = o.scrollHeight;") +
			new JsVar("e").set(new Js().jsId("sh_err"))+
			new Js("e.scrollTop = e.scrollHeight;")).code
		))
	)
}

class Document(ifaces : List[Iface]) extends Iface {
	private def getTags = ifaces.size match {
		case 0 => List()
		case _ => ifaces.head.merge(ifaces.tail)
	}

	def headCss(styles:Map[String,String]) = new Tag("head",
                /* FIXME: if system locale is not UTF-8, fromFile() will
                 * convert content to encoding specified in locale.
                 * That's why set it to UTF-8 or expect garbage in text
                 * file editor. :( */
		List(new Encoding("utf-8"), new CssBlock(styles),
			new Tag("script", js))
	)

	def tags = List(
		headCss(Map(
			"textarea"->"width:60em;height:90%",
			"#sh_in"->"width:60em",
			"#sh_out, #sh_err"->"height:10em",
			"body,textarea,input"->"background-color:black; color:white;",
			"a"->"color:yellow")),
		new Tag("body", getTags)
	)

	def js = ifaces.foldLeft("") {
		(a,v) => a + v.js
	}

	def html = new Tag("html", tags).toString
}
