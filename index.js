// Registered helper accumulates a list of 'lint'-like hints
CodeMirror.registerHelper("lint", "go", function(text) {
    var resobj = parse(text);
    var found = [];
    if (resobj.error != undefined) {
	var line   = resobj.error.line - 1;
	var column = resobj.error.column - 1;
	found.push({ from:     CodeMirror.Pos(line, column),
		     to:       CodeMirror.Pos(line, column + 1),
		     message:  resobj.error.msg,
		     severity: "error"
		   });
    }

    console.log("warnings: ", resobj.warnings);
    for (num in resobj.warnings) {
	var warning = resobj.warnings[num];
	console.log("warning: ", warning);
	found.push({ from:     CodeMirror.Pos(warning.line - 1, 1),
		     to:       CodeMirror.Pos(warning.line - 1, 1),
		     message:  warning.msg,
		     severity: "warning"
		   });
    };

    return found;
});

var editor = CodeMirror.fromTextArea(document.getElementById("texteditor"), {
    matchBrackets   : true,
    indentUnit      : 8,
    tabSize         : 8,
    indentWithTabs  : true,
    mode            : 'text/x-go',
    theme           : "eclipse",
    lineNumbers     : true,
    firstLineNumber : 0, 
    gutters         : ["CodeMirror-lint-markers",
		       "CodeMirror-linenumbers"],
    lint            : true
});

function msg(obj) {
    return obj.msg + ': line ' + obj.line + ', column ' + obj.column;
}

function analyze() {
    var result = document.getElementById('result');
    var resobj = interpret(editor.getValue());
    if (resobj.error != undefined) {
	result.innerText = msg(resobj.error);
    } else {
	console.log(resobj.result);
	result.innerHTML = '<pre>' + resobj.result + '</pre>';
    }
}

var abutton = document.getElementById('analyze');
abutton.addEventListener('click', analyze);

var sendrecvprog =
    "package main\n" +
    "\n" +
    "func main() {\n" +
    "	ch := make(chan int);\n" +
    "	go func() { ch <-42 }()\n" +
    "	var x int;\n" +
    "	x = <- ch;\n" +
    "	print(x)\n" +
    "}";

var nondetprog = "package main\n" +
    "\n" +
    "func main() {\n" +
    "	ch := make(chan int);\n" +
    "	go func() { ch <-2 }()\n" +
    "	go func() { ch <-3 }()\n" +
    "	var x int;\n" +
    "	var y int;\n" +
    "	x = <- ch;\n" +
    "	y = <- ch;\n" +
    "	print(x);\n" +
    "	print(y)\n" +
    "}";

var orderprog = "package main\n" +
    "\n" +
    "func main() {\n" +
    "	ch := make(chan int);\n" +
    "	go func() {\n" +
    "		ch <- 0;\n" +
    "		ch <- 1\n" +
    "	}()\n" +
    "	go func() { <-ch }()\n" +
    "	var y int;\n" +
    "	y = <-ch;\n" +
    "	print(y)\n" +
    "}";

var constantprog = "package main\n" +
    "\n" +
    "func main() {\n" +
    "	ch1 := make(chan int);\n" +
    "	ch2 := make(chan int);\n" +
    "	go func () {\n" +
    "		ch1 <-1;\n" +
    "		ch2 <-2;\n" +
    "	}()\n" +
    "	go func () {\n" +
    "		var x int;\n" +
    "		x = <-ch1;\n" +
    "		ch2 <- x+1;\n" +
    "	}()\n" +
    "	var y int;\n" +
    "	y = <-ch2;\n" +
    "//	print(y)\n" +
    "}";

var deadlockprog = "package main\n" +
    "\n" +
    "func main() {\n" +
    "	ch_a := make(chan int)\n" +
    "	ch_b := make(chan int)\n" +
    "	go func() {\n" +
    "		var x int;\n" +
    "		x = <- ch_a;\n" +
    "		ch_b <- 1;\n" +
    "		print(x)\n" +
    "	}()\n" +
    "	var y int;\n" +
    "	y = <- ch_b;\n" +
    "	ch_a <- 2;\n" +
    "	print(y)\n" +
    "}";

var deadlockgopherprog = "package main\n" +
    "\n" +
    "func main() {\n" +
    "	ch := make(chan int)\n" +
    "	done := make(chan int)\n" +
    "	go func() { ch <- 42 }() // Send\n" +
    "	go func() {              // Recv1\n" +
    "		var val int;\n" +
    "		val = <-ch;\n" +
    "		done <- val;\n" +
    "	}()\n" +
    "	go func() {              // Recv2\n" +
    "		var val int;\n" +
    "		val = <-ch;\n" +
    "		done <- val;\n" +
    "	}()\n" +
    "	go func() { for {} }()   // Work\n" +
    "	<-done;\n" +
    "	<-done\n" +
    "}";

var chainsprog = "package main\n" +
    "\n" +
    "func main() {\n" +
    "	ch0 := make(chan int);\n" +
    "	ch1 := make(chan int);\n" +
    "	go func() {\n" +
    "		var x int;\n" +
    "		for {\n" +
    "			x = <- ch0;\n" +
    "			ch1 <- (x + 1);\n" +
    "//			print(x)\n" +
    "		}\n" +
    "	}()\n" +
    "	var y int;\n" +
    "	ch0 <-1;\n" +
    "	for {\n" +
    "		y = <- ch1;\n" +
    "		ch0 <- (y + 1);\n" +
    "//		print(y)\n" +
    "	}\n" +
    "}";

var loopprog  = "package main\n" +
    "\n" +
    "func main() {\n" +
    "	ch := make(chan int);\n" +
    "	go func() {\n" +
    "		var x int;\n" +
    "		for {\n" +
    "			x = <- ch;\n" +
    "			print(x)\n" +
    "		}\n" +
    "	}()\n" +
    "	var x int; //int vars are initialized to 0\n" +
    "	for {\n" +
    "		ch <-x;\n" +
    "		x = x-1;\n" +
    "	}\n" +
    "}";

var loopextprog  = "package main\n" +
    "\n" +
    "func main() {\n" +
    "	ch := make(chan int);\n" +
    "	go func() {\n" +
    "		var x int;\n" +
    "		for 0 < x {\n" +
    "			x = <- ch;\n" +
    "			print(x)\n" +
    "		}\n" +
    "	}()\n" +
    "	var x int; //int vars are initialized to 0\n" +
    "	for {\n" +
    "		ch <-x;\n" +
    "		x = x-1;\n" +
    "	}\n" +
    "}";

var nontermination = "package main\n" +
    "\n" +
    "func main() {\n" +
    "	ch := make(chan int);\n" +
    "	go func() {\n" +
    "		var y int;\n" +
    "		y = 1000;\n" +
    "		for 0 < y {\n" +
    "			ch <- y;\n" +
    "			y = y-1;\n" +
    "		};\n" +
    "		print(y)\n" +
    "	}()\n" +
    "	var x int;\n" +
    "	x = 1;\n" +
    "	for 0 < x { x = <- ch }\n" +
    "}";

function progSelect() {
    var pselect = document.getElementById('prog-select');
    var choice = pselect.value;
    if (choice == 'send-recv')     { editor.setValue(sendrecvprog); }
    else if (choice == 'nondet')   { editor.setValue(nondetprog); }
    else if (choice == 'order')    { editor.setValue(orderprog); }
    else if (choice == 'constant') { editor.setValue(constantprog); }
    else if (choice == 'deadlock-simple') { editor.setValue(deadlockprog); }
    else if (choice == 'deadlock-gopherlyzer') { editor.setValue(deadlockgopherprog); }
    else if (choice == 'chains')   { editor.setValue(chainsprog); }
    else if (choice == 'loop')     { editor.setValue(loopprog); }
    else if (choice == 'loopext')  { editor.setValue(loopextprog); }
    else                           { editor.setValue(nontermination); }
    console.log("Selected value ", pselect.value);
};


var pselect = document.getElementById('prog-select');
pselect.addEventListener('change', progSelect);
