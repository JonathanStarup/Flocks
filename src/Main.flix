mod Flocks {
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
        wasWritten <- File.write(outputPath, doc);
        _ <- if (wasWritten) Result.Ok()
             else Result.Err("'${outputPath}' could not be created.")
    ) yield ()

def loadFromFile(inputPath: String): Failable[String] \ IO = {
    File.read(inputPath)
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

def parseArgs(args: List[String]): Failable[{inputPath = String, outputPath = String}] = {
    use GetOpt/ArgOrder.RequireOrder;
    use GetOpt/ArgDescr.{OptArg, ReqArg};

    let ordering = RequireOrder;
    let inputPathOption = {
        argDescriptor = ReqArg(Some, "The path of the api json file"),
        explanation = "The path of the api json file",
        optionIds = List#{'i'},
        optionNames = List#{"input"}
    };
    let outputPathOption = {
        argDescriptor = OptArg(identity, "The path of the output doc file"),
        explanation = "The path of the output doc file",
        optionIds = List#{'o'},
        optionNames = List#{"output"}
    };
    let optDescriptors = List#{outputPathOption, inputPathOption};
    let opt = GetOpt.getOpt(ordering, optDescriptors, args);
    match opt {
        case Success(some) =>
            ???
        case Failure(other) =>
            Nec.head(other) |> Err
    }
}

}
