module RobotName

open System

type Robot = {
  name: string
}

let createName () : string =
  let r = new Random()
  let rChar () : char = char (r.Next(65, 90))
  let rNumber () : int = r.Next(100, 1000)
  sprintf "%c%c%d" (rChar()) (rChar()) (rNumber())


let mkRobot () : Robot =
  { name = createName() }

let name (robot : Robot) : string =
  let { name = rName } = robot
  rName

let reset (robot : Robot) : Robot =
  { robot with name = createName() }