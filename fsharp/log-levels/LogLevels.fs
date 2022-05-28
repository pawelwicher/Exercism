module LogLevels

let infoLogLevelText = "[INFO]:"
let warningLogLevelText = "[WARNING]:"
let errorLogLevelText = "[ERROR]:"

let (|Info|Warning|Error|Unknown|) (logLine: string) =
    if logLine.StartsWith infoLogLevelText then Info
    elif logLine.StartsWith warningLogLevelText then Warning
    elif logLine.StartsWith errorLogLevelText then Error
    else Unknown

let trim (logLine: string) (logLevel: string) =
    logLine.TrimStart(logLevel.ToCharArray()).Trim()

let message (logLine: string): string =
    match logLine with
    | Info -> trim logLine infoLogLevelText
    | Warning -> trim logLine warningLogLevelText
    | Error -> trim logLine errorLogLevelText
    | _ -> ""

let logLevel (logLine: string): string =
    match logLine with
    | Info -> "info"
    | Warning -> "warning"
    | Error -> "error"
    | _ -> ""

let reformat (logLine: string): string =
    $"{message logLine} ({logLevel logLine})"