module SpaceAge (Planet(..), ageOn) where

data Planet = Earth
    | Mercury
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

earthYears :: Float -> Float
earthYears seconds = seconds / 31557600

ageOn :: Planet -> Float -> Float
ageOn planet seconds = let ey = earthYears seconds in
    case planet of
        Earth -> ey
        Mercury -> ey * (1/0.2408467)
        Venus -> ey * (1/0.61519726)
        Mars -> ey * (1/1.8808158)
        Jupiter -> ey * (1/11.862615)
        Saturn -> ey * (1/29.447498)
        Uranus -> ey * (1/84.016846)
        Neptune -> ey * (1/164.79132)
