module LuciansLusciousLasagna

let expectedMinutesInOven = 40

let remainingMinutesInOven minutes = 40 - minutes

let preparationTimeInMinutes layers = layers * 2

let elapsedTimeInMinutes layers minutes =
    preparationTimeInMinutes layers + minutes
