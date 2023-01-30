mod Flocs {
use Json.JsonElement

type alias Failable[t] = Result[String, t]

pub def runWithCommandLineArgs(): Unit \ IO = {
    let res = for (
        args <- parseArgs(Environment.getArgs());
        outputMsg <- run(args.inputPath, args.outputPath)
    ) yield outputMsg;
    exitWithResult(res)
}

pub def scenario(): Failable[String] \ IO = {
    run("personal/input.json", "personal/output.md")
}

pub def run(inputPath: String, outputPath: String): Failable[String] \ IO = {
    for (
        fileText <- loadFromFile(inputPath);
        json <- parseJson(fileText);
        doc <- generateDoc(json);
        _ <- outputToFile(outputPath, doc)
    ) yield "doc file was written to '${outputPath}'"
}

def parseJson(s: String): Failable[JsonElement] = {
    Json/Parse.parse(s) |> Option.toOk("Could not parse as json")
}

def generateDoc(_json: JsonElement): Failable[String] =
    "docDummy" |> Result.Ok

def outputToFile(outputPath: String, doc: String): Failable[Unit] \ IO =
    // delete if already present, then write
    for (
        _ <- File.deleteIfExists(outputPath);
        wasWritten <- File.write(outputPath, doc) |> Result.mapErr(msg -> "file write error: ${msg}");
        _ <- if (wasWritten) Result.Ok()
             else Result.Err("file write error: '${outputPath}' could not be created.")
    ) yield ()

def loadFromFile(inputPath: String): Failable[String] \ IO = {
    File.read(inputPath) |> Result.mapErr(msg -> "file read error: ${msg}")
}

def exitWithResult(res: Failable[String]): Unit \ IO = {
    import static java.lang.System.exit(Int32): Unit \ IO;
    match res {
        case Ok(msg) =>
            println(msg);
            exit(0)
        case Err(msg) =>
            println(msg);
            exit(-1)
    }
}

enum Flag {
    case Help
    case Output(String)
    case Input(String)
}

pub def parseArgs(args: List[String]): Failable[{inputPath = String, outputPath = String}] = {
    use GetOpt/ArgOrder.{RequireOrder, Permute};
    use GetOpt/ArgDescr.{OptArg, ReqArg, NoArg};
    use GetOpt.{getOpt, usageInfo};

    let inputPathOption = {
        argDescriptor = ReqArg(v -> Some(Input(v)), "<path>"),
        explanation = "The path of the api json file",
        optionIds = List#{'i'},
        optionNames = List#{"input"}
    };
    let outputPathOption = {
        argDescriptor = ReqArg(v -> Some(Output(v)), "<path>"),
        explanation = "The path of the output doc file",
        optionIds = List#{'o'},
        optionNames = List#{"output"}
    };
    let helpOption = {
        argDescriptor = NoArg(Help),
        explanation = "prints a helpful message",
        optionIds = List#{'h'},
        optionNames = List#{"help"}
    };
    let optDescriptors = List#{helpOption, outputPathOption, inputPathOption};
    let header = "Input and output must be given explicitely";
    match getOpt(Permute, optDescriptors, args) {
        case Validation.Success(optsRec) =>
            match (optsRec.nonOptions, optsRec.options) {
                case (Nil, opts) if getHelp(opts) => Result.Err(usageInfo(header, optDescriptors))
                case (Nil, opts) => extractArgs(opts) |> Option.toOk(usageInfo(header, optDescriptors))
                case _ => Result.Err(usageInfo(header, optDescriptors))
            }
            // Result.Err("%{opts}")
        case Validation.Failure(errs) =>
            Result.Err("${Nec.head(errs)}")
    }
}

def getHelp(l: List[Flag]): Bool = match l {
    case Nil => false
    case Cons(Help, _) => true 
    case Cons(_, xs) => getHelp(xs)
}

def extractArgs(l: List[Flag]): Option[{inputPath = String, outputPath = String}] = match l {
    case Cons(Input(i), Cons(Output(o), Nil)) => Some({inputPath = i, outputPath = o})
    case Cons(Output(o), Cons(Input(i), Nil)) => Some({inputPath = i, outputPath = o})
    case _ => None
}

}
