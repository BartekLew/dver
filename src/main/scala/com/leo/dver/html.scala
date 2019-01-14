package me.leo.dver

import java.io.File

import scala.io.Source._
import scala.sys.process._

import java.io.FileOutputStream

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

	def writeTo(inputId:String, text:Js) =
		new Js().call("ev.preventDefault", List()) +
			new JsVar("ed").set(new JsId(inputId)) +
			new JsVar("pos").set(new Js("ed.selectionStart")) +
			new Js("ed.value").set(
				new Js("ed.value.substring(0, pos)")
					.append(text.code).jsVar(
		"ed.value.substring(ed.selectionEnd, ed.value.length)"
				)
			) +
		new Js("ed.selectionStart = ed.selectionEnd = pos+1;")

	def literalTab(inputId:String) =
		new Js().cond(new Js("ev.keyCode == 9"),
			writeTo(inputId, new JsLiteral("\\t"))
		)

	def ru_map = Map(
		"`" -> "ё", "~" -> "Ё", "q" -> "й", "Q" -> "Й", "w" -> "ц", "W" -> "Ц",
		"e" -> "у", "E" -> "У", "r" -> "к", "R" -> "К", "t" -> "е", "T" -> "Е",
		"y" -> "н", "Y" -> "Н", "U" -> "Г", "u" -> "г", "I" -> "Ш", "i" -> "ш",
		"o" -> "щ", "O" -> "Щ", "p" -> "з", "P" -> "З", "[" -> "х", "{" -> "Х",
		"]" -> "ъ", "}" -> "Ъ", "a" -> "ф", "A" -> "Ф", "s" -> "ы", "S" -> "Ы",
		"d" -> "в", "D" -> "В", "f" -> "а", "F" -> "А", "g" -> "п", "G" -> "П",
		"h" -> "р", "H" -> "Р", "j" -> "о", "J" -> "О", "k" -> "л", "K" -> "Л",
		"l" -> "д", "L" -> "Д", ";" -> "ж", ":" -> "Ж", "'" -> "э", "\\\"" -> "Э",
		"z" -> "я", "Z" -> "Я", "x" -> "ч", "X" -> "Ч", "c" -> "с", "C" -> "С",
		"v" -> "м", "V" -> "М", "b" -> "и", "B" -> "И", "n" -> "т", "N" -> "Т",
		"m" -> "ь", "M" -> "Ь", "," -> "б", "<" -> "Б", "." -> "ю", ">" -> "Ю",
		"/" -> ".", "?" -> ",", "^" -> "?", "#" -> ":", "$" -> ";", "@" -> "\\\""
	) 

	def cyryllic(triggerId:String, outputId:String) = new JsHash("keymap", ru_map) +
		new JsVar("key").set(new Js("keymap[ev.key]")) +
		new Js().cond(
			new JsId(triggerId)->"checked" && new Js("key && !ev.ctrlKey"),
			writeTo(outputId, new Js("key"))
		);
}

class HtmlIface(content:List[Tag]) extends Iface {
	def tags = content
	def js = ""
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

	def js = (new JsVar("r").set(new JsId("upFile")->"files[0]") +
		new JsHttp("POST", new Js().literal("/w/" + cwd.getPath + "/")
				.jsVar("r.name"), new Js("r"),
				new Js("setTimeout(function(){window.location.reload(false);}, 1000);")
		)).asFun("uploadFile").code
	
}

class FileEditor(f:File) extends Iface {
	def contentEditor : Iface = new FSItem(f).mime.split("/")(0) match {
		case "image" => new ImageEditor(
			new ImageFile(f, new FSItem(f).imageTransformer))
		case "video" => new VideoEditor(f)
		case _ => new TextEditor(f)
	}

	def tags = List(
		new Tag("h3", Map(), Some("File: " + f.getPath + " " +
			new Tag("a", Map("href" -> ("/r/" + f.getParent)),
				Some("(parent directory)")).toString
	))) ++ contentEditor.tags

	def js = contentEditor.js
}

class VideoEditor(f:File) extends Iface {
	def tags = List(
		new Tag("video controls", Map("width"->"800", "height"->"600"),
			Some(new Tag("source", Map(
				"src"->("/R/" + f.getPath),
				"type"->(new FSItem(f).mime)),
				None
			).toString)
		)
	)

	def js = ""
}

class ImageEditor(f:ImageFile) extends Iface {
	def tags = List(
		new Tag("img", Map("src"->("/img/" + f.getPath),
			"id"->"img_disp"), None),
		new Tag("br")
	) ++ f.paramNames.sorted.map( p =>
		new Tag("input", Map(
			"placeholder"->p, "id"->("img_"+p),
			"onkeypress"->"img_up(event)", "value"->f.params.value(p)
		), None)
	)

	def js = new Js().cond(new Js("ev.keyCode == 13"),
			new JsHttp("POST", new JsLiteral("/img/" + f.getPath),
				f.params.keys.foldLeft(new Js())((a, p) =>
					a.separator(";").literal(p + "=").jsId("img_" + p)->"value"
				),
				new Js("window.location.reload(false);")
		)).asFun("img_up", "ev").code
}

class TextEditor(f:File) extends Iface {
	def tags = List(
		new Tag("input", Map(
			"type" -> "checkbox", "id" -> "writeCyryllic"
		), Some("cyryllic")), new Tag("br"),
		new Tag("textarea", Map("id" -> "texted",
				"onkeypress"->"onKey(event)"),
			Some(fileContent(f))),
		new Tag("br"),
		new Button("save", "updateFile()")
	)

	def js = new JsHttp("POST", new Js().literal("/w/" + f.getPath),
			new JsId("texted")->"value",
			new Js("alert(\"done\");")
		).asFun("updateFile").code +
		(literalTab("texted")
		+ cyryllic("writeCyryllic", "texted")
		).asFun("onKey", "ev").code
}

class Shell(path:String) extends Iface {
	def js = List("out", "err").map( stream => 
		new JsHttp("GET", new Js().literal("/R/" + path + "/box." + stream),
			new Js(),
			new JsVar("o").set(new JsId("sh_" + stream)) +
			new Js().cond(new Js("this.responseText.length != o.value.length"),
				new Js("o.value = this.responseText;") +
				new Js("o.scrollTop = o.scrollHeight")
			)
		)).foldLeft(new Js()) ((a,b) => a+b).asFun("refresh_output").code +
		(new Js().cond(new Js("ev.keyCode == 13"),
			new JsVar("cmd").set(new JsId("sh_in")) +
			new JsVar("out").set(new JsId("sh_out")) +
			new Js("refresh_output();") +
			new JsHttp("POST", new JsLiteral("/sh/" + path),
				new Js().jsVar("cmd.value"),
				new Js("setInterval(refresh_output, 2000);") +
				new Js("cmd.value").set(new JsLiteral(""))
		)) + literalTab("sh_in")).asFun("sh_cmd", "ev").code +
		new JsHttp("POST", new JsLiteral("/w/" + path + "/box.ctl"),
			new JsLiteral("k"), new Js("window.location.reload(false);")
		).asFun("sh_term").code +
		new JsHttp("POST", new JsLiteral("/w/" + path + "/box.ctl"),
			new JsLiteral("c"), new Js()
		).asFun("sh_cls").code +
		(new JsVar("day").set(new Js("new Date()")) +
		new JsHttp("POST", new JsLiteral("/sh/"),
			new JsLiteral("sudo date -s ")
				.append("day.getFullYear()")
				.literal("-")
				.append("(day.getMonth()+1)")
				.literal("-")
				.append("day.getDate()")
				.literal("\\\\ ")
				.append("day.getHours()")
				.literal(":")
				.append("day.getMinutes()"),
			new Js("alert(\"ok\");")
		)).asFun("sh_date").code

	def tags = List(
		new Tag("h4", Map(), Some("shell:")),
		new Tag("input", Map(
			"type"->"text", "id"->"sh_in",
			 "onkeypress"->"sh_cmd(event)"
		)),
		new Tag("br"),
		new Button("clear", "sh_cls()"),
		new Button("terminate", "sh_term()"),
		new Button("set date", "sh_date()"),
		new Tag("br")
	) ++ (List("out", "err").map( stream => List(
		new Tag("textarea readonly", Map("id"->("sh_" + stream)),
			Some(fileContent(new File(path + "/box." + stream)))
		),
		new Tag("script", Map(), Some(
			(new JsVar("o").set(new JsId("sh_" + stream)) +
			new Js("o.scrollTop = o.scrollHeight;")).code
		))
	)).foldLeft(List[Tag]()) ((a,b)=>a++b))
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
			"textarea"->"width:70em;height:90%",
			"#sh_in"->"width:120ex",
			"#sh_out, #sh_err"->"width:49%;height:90%",
			"body,textarea,input"->"background-color:black; color:white;",
			"a"->"color:yellow")),
		new Tag("body", getTags)
	)

	def js = ifaces.foldLeft("") {
		(a,v) => a + v.js
	}

	def html = new Tag("html", tags).toString
}
